;;; pgmacs.el --- Emacs is editing a PostgreSQL database  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Eric Marsden
;; Author: Eric Marsden <eric.marsden@risk-engineering.org>
;; Version: 0.5
;; Package-Requires: ((emacs "29.1") (pg "0.32"))
;; URL: https://github.com/emarsden/pgmacs/
;; Keywords: data, PostgreSQL, database
;; SPDX-License-Identifier: GPL-2.0-or-later

;;; Commentary:
;;
;; See README.md at https://github.com/emarsden/pgmacs/


;;; Code:

(require 'cl-lib)
(require 'button)
(require 'widget)
(require 'wid-edit)
(require 'pg)
(require 'pgmacstbl)

(defgroup pgmacs nil
  "Edit a PostgreSQL database from Emacs."
  :prefix "pgmacs-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/emarsden/pgmacs/"))

(defface pgmacs-table-data
  '((t (:inherit fixed-pitch-serif
        :foreground "black")))
  "Face used to display data in a PGmacs database table."
  :group 'pgmacs)

(defface pgmacs-table-header
  '((t (:inherit bold
        :weight bold
        :foreground "black")))
  "Face used to display a PGmacs database table header."
  :group 'pgmacs)

(defcustom pgmacs-row-colors
  '("#D9CEB4" "#D9B96C")
  "The colors used for alternating rows in a database table."
  :type '(list color color)
  :group 'pgmacs)

(defcustom pgmacs-row-limit 1000
  "The maximum number of rows to retrieve per database query.
If more rows are present in the PostgreSQL query result, the display of results
will be paginated.  You may wish to set this to a low value if accessing
PostgreSQL over a slow network link."
  :type 'number
  :group 'pgmacs)

(defcustom pgmacs-mode-hook nil
  "Mode hook for `pgmacs-mode'."
  :type 'hook
  :group 'pgmacs)

(defvar pgmacs-mode-map (make-sparse-keymap))

(keymap-set pgmacs-mode-map (kbd "q") 'bury-buffer)
(keymap-set pgmacs-mode-map (kbd "h") 'pgmacs--table-list-help)
(keymap-set pgmacs-mode-map (kbd "r") 'pgmacs--table-list-redraw)
(keymap-set pgmacs-mode-map (kbd "e") 'pgmacs-run-sql)

(defun pgmacs-mode ()
  "Major mode for editing PostgreSQL database."
  (setq major-mode 'pgmacs-mode
        mode-name "PGmacs")
  ;; Not appropriate for user to type stuff into our buffers.
  (put 'pgmacs-mode 'mode-class 'special)
  (use-local-map pgmacs-mode-map)
  (run-mode-hooks 'pgmacs-mode-hook))

(defvar pgmacs-transient-map (make-sparse-keymap))
(keymap-set pgmacs-transient-map (kbd "q") 'kill-buffer)

(define-minor-mode pgmacs-transient-mode
  "Minor mode for transient PGmacs buffers."
  :global nil
  :init-value nil
  :keymap pgmacs-transient-map)

(defvar pgmacs-paginated-map (make-sparse-keymap))
(keymap-set pgmacs-paginated-map (kbd "n") 'pgmacs--paginated-next)
(keymap-set pgmacs-paginated-map (kbd "p") 'pgmacs--paginated-prev)

(define-minor-mode pgmacs-paginated-mode
  "Minor mode for paginated PGmacs table buffers."
  :global nil
  :init-value nil
  :keymap pgmacs-paginated-map)


;; Used for updating on progress retrieving information from PostgreSQL.
(defvar pgmacs--progress nil)
(defvar pgmacs--progress-timer nil)

(defun pgmacs--start-progress-reporter (msg)
  "Create a progress reporter that displays message MSG."
  (setq pgmacs--progress (make-progress-reporter msg))
  (setq pgmacs--progress-timer
        (run-with-timer 0.2 0.2 (lambda ()
                                  (when pgmacs--progress
                                    (progress-reporter-update pgmacs--progress))))))

(defun pgmacs--update-progress (msg)
  "Update the progress reporter with message MSG."
  (when pgmacs--progress
    (progress-reporter-update msg)))

(defun pgmacs--stop-progress-reporter ()
  "Stop the progress reporter."
  (when pgmacs--progress
    (progress-reporter-done pgmacs--progress))
  (when pgmacs--progress-timer
    (cancel-timer pgmacs--progress-timer)
    (setq pgmacs--progress-timer nil))
  (when pgmacs--progress
    (setq pgmacs--progress nil)))


(defvar-local pgmacs--kill-ring
    "Used for copying and pasting rows in a buffer's table."
  nil)

(defvar-local pgmacs--con nil)
(defvar-local pgmacs--table nil)
(defvar-local pgmacs--column-type-names nil)
(defvar-local pgmacs--offset nil)


(defun pgmacs--notify (fmt &rest args)
  "Display a notification regarding PGmacs activity.
Applies format string FMT to ARGS."
  (message (concat "PostgreSQL> " (apply #'format (cons fmt args)))))

(defun pgmacs--value-formatter-not-null (type-name)
  "Return a function that formats a non-NULL value of type TYPE-NAME."
  (cond ((string= type-name "date")
         (lambda (val) (format-time-string "%Y-%m-%d" val)))
        ((or (string= type-name "timestamp")
            (string= type-name "timestamptz")
	    (string= type-name "datetime"))
         ;; these are represented as a `decode-time' structure
         (lambda (val) (format-time-string "%Y-%m-%dT%T" val)))
        ((or (string= type-name "text")
             (string= type-name "varchar")
             (string= type-name "name"))
         (lambda (s) (or s "")))
        ((string= type-name "bpchar")
         #'byte-to-string)
        ((string= type-name "hstore")
         (lambda (ht)
           (let ((items (list)))
             (maphash (lambda (k v) (push (format "\"%s\"=>\"%s\"" k v) items)) ht)
             (string-join items ","))))
        ((string= type-name "json")
         #'json-serialize)
        (t
         (lambda (val) (format "%s" val)))))

(defun pgmacs--value-formatter (type-name)
  "Return a function that formats a value of type TYPE-NAME."
  (let ((fmt (pgmacs--value-formatter-not-null type-name)))
    (lambda (val) (if val (funcall fmt val) ""))))

(defun pgmacs--value-width (type-name)
  "Width for a column containing elements of type TYPE-NAME."
  (cond ((string= type-name "smallint") 4)
        ((string= type-name "int2") 4)
        ((string= type-name "int4") 6)
        ((string= type-name "int8") 10)
        ((string= type-name "oid") 10)
        ((string= type-name "bool") 4)
        ((string= type-name "bit") 4)
        ((string= type-name "varbit") 10)
        ((string= type-name "char") 4)
        ((string= type-name "bpchar") 4)
        ((string= type-name "char2") 4)
        ((string= type-name "char4") 6)
        ((string= type-name "char8") 10)
        ((string= type-name "char16") 20)
        ((string= type-name "text") 25)
        ((string= type-name "varchar") 25)
        ((string= type-name "name") 25)
        ((string= type-name "bytea") 10)
        ((string= type-name "json") 20)
        ((string= type-name "jsonb") 20)
        ((string= type-name "hstore") 20)
        ((string= type-name "numeric") 10)
        ((string= type-name "float4") 10)
        ((string= type-name "float8") 10)
        ((string= type-name "date") 16)
        ((string= type-name "timestamp") 20)
        ((string= type-name "timestamptz") 20)
        ((string= type-name "datetime") 20)
        ((string= type-name "time") 12)
        ((string= type-name "reltime") 10)
        ((string= type-name "timespan") 12)
        (t 10)))

(defun pgmacs--alignment-for (type-name)
  "Return the alignment for type TYPE-NAME, either \='left or \='right."
  (cond ((string= type-name "smallint") 'right)
        ((string= type-name "int2") 'right)
        ((string= type-name "int4") 'right)
        ((string= type-name "int8") 'right)
        ((string= type-name "oid") 'right)
        ((string= type-name "bool") 'left)
        ((string= type-name "bit") 'right)
        ((string= type-name "varbit") 'right)
        (t 'left)))

;; Name may be a qualified name or a simple string. Transform this into a string for display to the
;; user. We only want to show the outer ?" characters if they are necessary (if some characters in
;; the identifier require quoting).
(defun pgmacs--display-identifier (name)
  "Return the identifier NAME in a form suitable for display to the user."
  (cl-labels ((safe-p (ch)
                (string-match-p "[0-9a-zA-Z_]" (string ch)))
              (user-facing (nm)
                (if (cl-every #'safe-p nm)
                    nm
                  (pg-escape-identifier nm))))
    (cond ((pg-qualified-name-p name)
           (let ((schema (pg-qualified-name-schema name))
                 (name (pg-qualified-name-name name)))
             (if schema
                 (format "%s.%s"
                         (user-facing schema)
                         (user-facing name))
               (user-facing name))))
          (t
           (user-facing name)))))

(defun pgmacs--row-as-json (current-row)
  "Copy the CURRENT-ROW as JSON to the kill ring."
  (unless (json-available-p)
    (error "Emacs is not compiled with JSON support"))
  (let* ((pgmacstbl (pgmacstbl-current-table))
         (cols (pgmacstbl-columns pgmacstbl))
         (ht (make-hash-table :test #'equal)))
    (cl-loop
     for col in cols
     for v in current-row
     do (puthash (pgmacstbl-column-name col) v ht))
    (kill-new (json-serialize ht))
    (message "JSON copied to kill ring")))

(defun pgmacs--read-value-minibuffer (name type prompt current-value)
  "Read a value for column NAME in the minibuffer using PROMPT.
The column has SQL type TYPE and has current value CURRENT-VALUE."
  (let* ((prompt (format prompt name type)))
    (read-string prompt (format "%s" current-value))))

;; TODO: perhaps if the field value is very long or of BYTEA type, prompt "really want to edit in
;; minibuffer" and suggest using the widget editing mode instead.
(defun pgmacs--read-value (name type prompt current-value)
  "Read a value for column NAME of SQL type TYPE.
Use PROMPT in the minibuffer and show the current value CURRENT-VALUE."
  (let* ((user-provided (pgmacs--read-value-minibuffer name type prompt current-value))
         (parser (pg-lookup-parser type))
         (ce (pgcon-client-encoding pgmacs--con)))
    (if parser (funcall parser user-provided ce) user-provided)))

(defun pgmacs--edit-value-minibuffer (row primary-keys)
  "Edit and update in PostgreSQL the column value at point.
The new value in database row ROW is read in the minibuffer.
Editing requires the database table to have primary keys named in the list
PRIMARY-KEYS."
  (when (null primary-keys)
    (error "Can't edit content of a table that has no PRIMARY KEY"))
  (let* ((pgmacstbl (or (pgmacstbl-current-table)
                     (error "Cursor is not in a pgmacstbl")))
         (current-row (or (pgmacstbl-current-object)
                          (error "Cursor is not on a pgmacstbl row")))
         (cols (pgmacstbl-columns pgmacstbl))
         (col-id (or (pgmacstbl-current-column)
                     (error "Not on a pgmacstbl column")))
         (col (nth col-id cols))
         (col-name (pgmacstbl-column-name col))
         (col-type (aref pgmacs--column-type-names col-id))
         (pk (cl-first primary-keys))
         (pk-col-id (or (cl-position pk cols :key #'pgmacstbl-column-name :test #'string=)
                        (error "Can't find primary key %s in the pgmacstbl column list" pk)))
         (pk-col-type (and pk-col-id (aref pgmacs--column-type-names pk-col-id)))
         (pk-value (and pk-col-id (nth pk-col-id row))))
    (unless pk-value
      (error "Can't find value for primary key %s" pk))
    (let* ((current (funcall (pgmacstbl-column-formatter col)
                             (nth col-id current-row)))
           (new-value (pgmacs--read-value (substring-no-properties col-name)
                                          (substring-no-properties col-type)
                                          "Change %s (%s) to: "
                                          current))
           (sql (format "UPDATE %s SET %s = $1 WHERE %s = $2"
                        (pg-escape-identifier pgmacs--table)
                        (pg-escape-identifier col-name)
                        (pg-escape-identifier pk)))
           (res (pg-exec-prepared pgmacs--con sql
                                  `((,new-value . ,col-type)
                                    (,pk-value . ,pk-col-type)))))
      (pgmacs--notify "%s" (pg-result res :status))
      (let ((new-row (copy-sequence current-row)))
        (setf (nth col-id new-row) new-value)
        ;; pgmacstbl-update-object doesn't work, so insert then delete old row
        (pgmacstbl-insert-object pgmacstbl new-row current-row)
        (pgmacstbl-remove-object pgmacstbl current-row)
        ;; redrawing is necessary to ensure that all keybindings are present for the newly inserted
        ;; row.
        (pgmacs--redraw-pgmacstbl)))))

(defun pgmacs--widget-for (type current-value)
  "Create a widget for TYPE and CURRENT-VALUE in the current buffer."
  (cond ((string= "bool" type)
         (widget-create 'boolean current-value))
        ((or (string= "smallint" type)
             (string= "int2" type)
             (string= "int4" type)
             (string= "int8" type)
             (string= "oid" type))
         (widget-create 'integer current-value))
        ((or (string= type "numeric")
             (string= type "float4")
             (string= type "float8"))
         (widget-create 'float current-value))
        ((or (string= type "char")
             (string= type "bpchar"))
         (widget-create 'character current-value))
        (t
         (widget-create 'editable-field
                        :size (min 200 (+ 5 (length current-value)))
                        (format "%s" current-value)))))

(defun pgmacs--edit-value-widget (row primary-keys)
  "Edit and update in PostgreSQL the value at point in ROW.
Uses a dedicated widget buffer.  Editing is only possible if the current table
has primary keys, named in the list PRIMARY-KEYS."
  (when (null primary-keys)
      (error "Can't edit content of a table that has no PRIMARY KEY"))
  (let* ((con pgmacs--con)
         (table pgmacs--table)
         (pgmacstbl (pgmacstbl-current-table))
         (current-row (pgmacstbl-current-object))
         (cols (pgmacstbl-columns pgmacstbl))
         (col-id (pgmacstbl-current-column))
         (col (nth col-id cols))
         (col-name (pgmacstbl-column-name col))
         (col-type (aref pgmacs--column-type-names col-id))
         (pk (cl-first primary-keys))
         (pk-col-id (cl-position pk cols :key #'pgmacstbl-column-name :test #'string=))
         (pk-col-type (aref pgmacs--column-type-names pk-col-id))
         (pk-value (and pk-col-id (nth pk-col-id row))))
    (unless pk-value
      (error "Can't find value for primary key %s" pk))
    (let* ((current (funcall (pgmacstbl-column-formatter col)
                             (nth col-id current-row)))
           (sql (format "UPDATE %s SET %s = $1 WHERE %s = $2"
                        (pg-escape-identifier pgmacs--table)
                        (pg-escape-identifier col-name)
                        (pg-escape-identifier pk)))
           (updater (lambda (user-provided)
                      (let* ((parser (pg-lookup-parser col-type))
                             (ce (pgcon-client-encoding pgmacs--con))
                             ;; Some of the input widgets we use return a pre-parsed type (e.g. a
                             ;; floating point number) rather than a string
                             (new-value (if (and (stringp user-provided) parser)
                                            (funcall parser user-provided ce)
                                          user-provided))
                             (res (pg-exec-prepared pgmacs--con sql
                                                    `((,new-value . ,col-type)
                                                      (,pk-value . ,pk-col-type)))))
                        (pgmacs--notify "%s" (pg-result res :status))
                        (let ((new-row (copy-sequence current-row)))
                          (setf (nth col-id new-row) new-value)
                          ;; pgmacstbl-update-object doesn't work, so insert then delete old row
                          (pgmacstbl-insert-object pgmacstbl new-row current-row)
                          (pgmacstbl-remove-object pgmacstbl current-row)
                          (pgmacs--redraw-pgmacstbl))))))
      (switch-to-buffer "*PGmacs update widget*")
      (erase-buffer)
      (remove-overlays)
      (kill-all-local-variables)
      (pgmacs-mode)
      (setq-local pgmacs--con con
                  pgmacs--table table)
      (widget-insert (propertize (format "Update column %s" col-name) 'face 'bold))
      (widget-insert "\n\n")
      (widget-insert (format "Change %s (type %s) to:" col-name col-type))
      (widget-insert "\n\n")
      (let* ((w-updated
              (progn
                ;; (widget-insert (format "%12s: " "New value"))
                (pgmacs--widget-for col-type current))))
        (widget-insert "\n\n")
        (widget-create 'push-button
                       :notify (lambda (&rest _ignore)
                                 (kill-buffer (current-buffer))
                                 (funcall updater (widget-value w-updated)))
                       "Update")
        (widget-insert "\n\n\nTo abort editing the column, simply kill this buffer.\n")
        (use-local-map widget-keymap)
        (widget-setup)
        (goto-char (point-min))
        (widget-forward 1)))))

(defun pgmacs--delete-row (row primary-keys)
  "Delete ROW from the current table.
Modifying the PostgreSQL database is only possible when the current table has
primary keys, whose names are given by the list PRIMARY-KEYS."
  (when (null primary-keys)
    (error "Can't edit content of a table that has no PRIMARY KEY"))
  (when (y-or-n-p (format "Really delete PostgreSQL row %s?" row))
    (let* ((pgmacstbl (pgmacstbl-current-table))
           (cols (pgmacstbl-columns pgmacstbl))
           (pk (cl-first primary-keys))
           (pk-col-id (cl-position pk cols :key #'pgmacstbl-column-name :test #'string=))
           (pk-col-type (and pk-col-id (aref pgmacs--column-type-names pk-col-id)))
           (pk-value (and pk-col-id (nth pk-col-id row))))
      (unless pk-value
        (error "Can't find value for primary key %s" pk))
      (let* ((res (pg-exec-prepared
                   pgmacs--con
                   (format "DELETE FROM %s WHERE %s = $1"
                           (pg-escape-identifier pgmacs--table)
                           (pg-escape-identifier pk))
                   `((,pk-value . ,pk-col-type)))))
        (pgmacs--notify "%s" (pg-result res :status)))
      (pgmacstbl-remove-object pgmacstbl row)
      (pgmacs--redraw-pgmacstbl))))

(defun pgmacs--insert-row (current-row)
  "Insert a new row of data into the current table after CURRENT-ROW.
Uses the minibuffer to prompt for new values."
  ;; TODO we need to handle the case where there is no existing pgmacstbl because the underlying SQL
  ;; table is empty.
  (let* ((pgmacstbl (pgmacstbl-current-table))
         (cols (pgmacstbl-columns pgmacstbl))
         (col-names (list))
         (values (list))
         (value-types (list)))
    (dolist (col cols)
      (let* ((col-name (pgmacstbl-column-name col))
             (col-id (cl-position col-name cols :key #'pgmacstbl-column-name :test #'string=))
             (col-type (aref pgmacs--column-type-names col-id))
             (col-has-default (not (null (pg-column-default pgmacs--con pgmacs--table col-name)))))
        (unless col-has-default
          (let* ((current (nth col-id current-row))
                 (val (pgmacs--read-value col-name col-type "Value for column %s (%s): " current)))
            (push col-name col-names)
            (push val values)
            (push col-type value-types)))))
    (let* ((placeholders (cl-loop for i from 1 to (length values)
                                  collect (format "$%d" i)))
           (target-cols (mapcar #'pg-escape-identifier col-names))
           (res (pg-exec-prepared
                 pgmacs--con
                 (format "INSERT INTO %s(%s) VALUES(%s)"
                         (pg-escape-identifier pgmacs--table)
                         (string-join target-cols ",")
                         (string-join placeholders ","))
                 (cl-loop for v in values
                          for vt in value-types
                          collect (cons v vt)))))
      (pgmacs--notify "%s" (pg-result res :status))
      ;; It's tempting to use pgmacstbl-insert-object here to avoid a full refresh of the pgmacstbl.
      ;; However, we don't know what values were chosen for any columns that have a default, so we
      ;; need to refetch the data from PostgreSQL.
      (pgmacs--display-table pgmacs--table))))

(defun pgmacs--insert-row-widget (current-row)
  "Insert a new row of data into the current table after CURRENT-ROW.
Uses a widget-based buffer to prompt for new values.  Updates the
PostgreSQL database."
  (let* ((con pgmacs--con)
         (table pgmacs--table)
         (ce (pgcon-client-encoding pgmacs--con))
         (pgmacstbl (pgmacstbl-current-table))
         (cols (pgmacstbl-columns pgmacstbl))
         (editable-cols (list))
         (col-names (list))
         (col-types (list)))
    ;; Determine which of the columns are editable (those that do not have a defined default)
    (dolist (col cols)
      (let* ((col-name (pgmacstbl-column-name col))
             (col-id (cl-position col-name cols :key #'pgmacstbl-column-name :test #'string=))
             (current (nth col-id current-row))
             (col-type (aref pgmacs--column-type-names col-id))
             (col-has-default (not (null (pg-column-default pgmacs--con pgmacs--table col-name)))))
        (unless col-has-default
          (push (vector col-name col-type current) editable-cols)
          (push col-name col-names)
          (push col-type col-types))))
    (setq editable-cols (nreverse editable-cols)
          col-names (nreverse col-names)
          col-types (nreverse col-types))
    (let* ((widgets (list))
           (placeholders (cl-loop for i from 1 to (length editable-cols)
                                  collect (format "$%d" i)))
           (target-cols (mapcar #'pg-escape-identifier col-names))
           (updater (lambda (user-provided-values)
                      (let* ((values (cl-loop
                                      for v in user-provided-values
                                      for vt in col-types
                                      with parser = (pg-lookup-parser vt)
                                      collect (cons (if parser (funcall parser v ce) v) vt)))
                             (sql (format "INSERT INTO %s(%s) VALUES(%s)"
                                          (pg-escape-identifier pgmacs--table)
                                          (string-join target-cols ",")
                                          (string-join placeholders ",")))
                             (res (pg-exec-prepared pgmacs--con sql values)))
                        (pgmacs--notify "%s" (pg-result res :status))
                        ;; It's tempting to use pgmacstbl-insert-object here to avoid a full refresh of
                        ;; the pgmacstbl. However, we don't know what values were chosen for any columns
                        ;; that have a default.
                        (pgmacs--display-table table)))))
      (switch-to-buffer "*PGmacs insert row widget*")
      (erase-buffer)
      (remove-overlays)
      (kill-all-local-variables)
      (setq-local pgmacs--con con
                  pgmacs--table table)
      (widget-insert (propertize (format "Insert row into table %s" table) 'face 'bold))
      (widget-insert "\n\n")
      (dolist (ecv editable-cols)
        (let ((name (aref ecv 0))
              (type (aref ecv 1)))
          (widget-insert (format "\n%7s (SQL type %s): " name type))
          (push (pgmacs--widget-for type "") widgets)))
      (setq widgets (nreverse widgets))
      (widget-insert "\n\n")
      (widget-create 'push-button
                     :notify (lambda (&rest _ignore)
                               (funcall updater (mapcar #'widget-value widgets))
                               (kill-buffer (current-buffer)))
                     "Insert row")
      (widget-insert "\n")
      (use-local-map widget-keymap)
      (widget-setup)
      (goto-char (point-min))
      (widget-forward 1))))

(defun pgmacs--copy-row (current-row)
  "Copy CURRENT-ROW to the PGMacs internal kill ring."
  (setq pgmacs--kill-ring (cons pgmacs--table current-row))
  (message "Row copied to PGmacs kill ring"))

;; Insert new row at current position based on content of our "kill ring".
(defun pgmacs--yank-row (_current-row)
  "Insert a new row into the current table after the current row.
The new row contents are based on the last copied row. Columns for which
a default SQL value is defined (such as a SERIAL type) will take the
default value instead of the last copied value."
  (unless pgmacs--kill-ring
    (error "PGmacs kill ring is empty"))
  (unless (eq (car pgmacs--kill-ring) pgmacs--table)
    (error "Can't paste into a different PostgreSQL table"))
  (message "Pasting row from PGmacs kill ring")
  ;; Insert a new row based on the copied row, but without specifying values for the columns that
  ;; have a default value
  (let* ((yanked-row (cdr pgmacs--kill-ring))
         (pgmacstbl (pgmacstbl-current-table))
         (cols (pgmacstbl-columns pgmacstbl))
         (col-names (list))
         (values (list))
         (value-types (list)))
    (cl-loop
     for col in cols
     for pasted-val in yanked-row
     do (let* ((col-name (pgmacstbl-column-name col))
               (col-id (cl-position col-name cols :key #'pgmacstbl-column-name :test #'string=))
               (col-type (aref pgmacs--column-type-names col-id))
               (col-has-default (not (null (pg-column-default pgmacs--con pgmacs--table col-name)))))
          (unless col-has-default
            (push col-name col-names)
            (push pasted-val values)
            (push col-type value-types))))
    (let* ((placeholders (cl-loop for i from 1 to (length values)
                                  collect (format "$%d" i)))
           (target-cols (mapcar #'pg-escape-identifier col-names))
           (res (pg-exec-prepared
                 pgmacs--con
                 (format "INSERT INTO %s(%s) VALUES(%s)"
                         (pg-escape-identifier pgmacs--table)
                         (string-join target-cols ",")
                         (string-join placeholders ","))
                 (cl-loop for v in values
                          for vt in value-types
                          collect (cons v vt)))))
      (pgmacs--notify "%s" (pg-result res :status))
      ;; It's tempting to use pgmacstbl-insert-object here to avoid a full refresh of the pgmacstbl.
      ;; However, we don't know what values were chosen for any columns that have a default.
      ;; This means that we can't insert at the current-row position.
      (pgmacs--display-table pgmacs--table))))


;; We can also SELECT c.column_name, c.data_type
(defun pgmacs--table-primary-keys (con table)
  "Return the columns active as PRIMARY KEY in TABLE.
Uses PostgreSQL connection CON."
  (let* ((schema (if (pg-qualified-name-p table)
                     (pg-qualified-name-schema table)
                   "public"))
         (tname (if (pg-qualified-name-p table)
                    (pg-qualified-name-name table)
                  table))
         (sql "SELECT c.column_name
      FROM information_schema.table_constraints tc
      JOIN information_schema.constraint_column_usage AS ccu USING (constraint_schema, constraint_name)
      JOIN information_schema.columns AS c ON c.table_schema = tc.constraint_schema
      AND tc.table_name = c.table_name AND ccu.column_name = c.column_name
      WHERE constraint_type = 'PRIMARY KEY' AND tc.table_schema=$1 AND tc.table_name = $2")
         (res (pg-exec-prepared con sql `((,schema . "text") (,tname . "text")))))
    (mapcar #'cl-first (pg-result res :tuples))))

(defun pgmacs--column-nullable-p (con table column)
  "Is there a NOT NULL constraint on COLUMN in TABLE?
Uses PostgreSQL connection CON."
  (let* ((schema (if (pg-qualified-name-p table)
                     (pg-qualified-name-schema table)
                   "public"))
         (tname (if (pg-qualified-name-p table)
                    (pg-qualified-name-name table)
                  table))
         (sql "SELECT 1 from information_schema.columns
               WHERE table_schema=$1 AND table_name=$2 AND column_name=$3 AND is_nullable='YES'")
         (params `((,schema . "text") (,tname . "text") (,column . "text")))
         (res (pg-exec-prepared con sql params)))
    (null (pg-result res :tuples))))

(defun pgmacs--column-info (con table column)
  "Return a string containing metainformation on COLUMN in TABLE.
The metainformation includes the type name, whether the column is a PRIMARY KEY,
whether it is affected by constraints such as UNIQUE.  Information is retrieved
over the PostgreSQL connection CON."
  (let* ((schema (if (pg-qualified-name-p table)
                     (pg-qualified-name-schema table)
                   "public"))
         (tname (if (pg-qualified-name-p table)
                    (pg-qualified-name-name table)
                  table))
         (sql "SELECT tc.constraint_type FROM information_schema.table_constraints tc
               JOIN information_schema.constraint_column_usage AS ccu USING (constraint_schema, constraint_name)
               JOIN information_schema.columns AS c ON c.table_schema = tc.constraint_schema
               AND tc.table_name = c.table_name
               AND ccu.column_name = c.column_name
               WHERE tc.constraint_schema = $1 AND tc.table_name = $2 AND c.column_name = $3")
         (params `((,schema . "text") (,tname . "text") (,column . "text")))
         (res (pg-exec-prepared con sql params))
         (constraints (pg-result res :tuples))
         (res (pg-exec-prepared
               con
               "SELECT character_maximum_length FROM information_schema.columns
                WHERE table_schema=$1 AND table_name=$2 AND column_name=$3"
               `((,schema . "text") (,tname . "text") (,column . "text"))))
         (maxlen (pg-result res :tuple 0))
         (defaults (pg-column-default con table column))
         (sql (format "SELECT %s FROM %s LIMIT 0"
                      (pg-escape-identifier column)
                      (pg-escape-identifier table)))
         (res (pg-exec con sql))
         (oid (cadar (pg-result res :attributes)))
         (type-name (pg--lookup-type-name oid))
         (column-info (list type-name)))
    (dolist (c constraints)
      (push (cl-first c) column-info))
    (when (pgmacs--column-nullable-p con table column)
      (push "NOT NULL" column-info))
    (when (cl-first maxlen)
      (push (format "max-len %s" (cl-first maxlen)) column-info))
    (when defaults
      (push (format "DEFAULT %s" defaults) column-info))
    (string-join (reverse column-info) ", ")))

;; TODO also include VIEWs
;;   SELECT * FROM information_schema.views
;;
;; The count of table rows using COUNT(*) is imperfect for a number of reasons: it's not using a
;; parameterized query (not possible for a DDL query), and it's slow on large tables. However,
;; alternatives are probably less good because they return incorrect results for tables that haven't
;; yet been VACCUMed. Possible alternatives:
;;
;;    SELECT reltuples::bigint FROM pg_class WHERE oid=$1::regclass (returns -1)
;;
;;    SELECT n_live_tup FROM pg_stat_user_tables (zero for non-VACUUMed tables)
(defun pgmacs--list-tables ()
  "Return a list of table-names and associated metadata for the current database.
Table names are schema-qualified if the schema is non-default."
  (let ((entries (list)))
    (dolist (table (pg-tables pgmacs--con))
      (let* ((tname (pg-escape-identifier table))
             (sql (format "SELECT COUNT(*), pg_size_pretty(pg_total_relation_size($1)) FROM %s"
                          tname))
             (res (pg-exec-prepared pgmacs--con sql `((,tname . "text"))))
             (rows (cl-first (pg-result res :tuple 0)))
             (size (cl-second (pg-result res :tuple 0)))
             (owner (pg-table-owner pgmacs--con table))
             (comment (pg-table-comment pgmacs--con table)))
        (push (list table rows size owner (or comment "")) entries)))
    entries))


(defun pgmacs--make-column-displayer (metainfo)
  "Return a display function which echos METAINFO in minibuffer."
  (lambda (fvalue max-width _table)
    (let ((truncated (if (> (string-pixel-width fvalue) max-width)
                         ;; TODO could include the ellipsis here
                         (pgmacstbl--limit-string fvalue max-width)
                       fvalue)))
      (propertize truncated 'face 'pgmacs-table-data 'help-echo metainfo))))

(defun pgmacs--table-to-csv (&rest _ignore)
  "Dump the current PostgreSQL table in CSV format into an Emacs buffer."
  (let* ((con pgmacs--con)
         (table pgmacs--table)
         (t-id (pg-escape-identifier table))
         (t-pretty (pgmacs--display-identifier table))
         (buf (get-buffer-create (format "*PostgreSQL CSV for %s*" t-pretty)))
         (sql (format "COPY %s TO STDOUT WITH (FORMAT CSV)" t-id)))
    (pop-to-buffer buf)
    (pgmacs-transient-mode)
    (pg-copy-to-buffer con sql buf)
    (goto-char (point-min))))

(defun pgmacs--add-primary-key (&rest _ignore)
  "Add a PRIMARY KEY to the current PostgreSQL table."
  (let ((pk (pgmacs--table-primary-keys pgmacs--con pgmacs--table)))
    (when pk
      (error "Table %s already has a primary key %s" (pgmacs--display-identifier pgmacs--table) pk)))
  (cl-flet ((exists (name) (cl-find name (pg-columns pgmacs--con pgmacs--table) :test #'string=)))
    (let* ((colname (or (cl-find-if-not #'exists (list "id" "idpk" "idcol" "pk" "_id" "newpk"))
                        (error "Can't autogenerate a name for primary key")))
           (sql (format "ALTER TABLE %s ADD COLUMN %s BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY"
                        (pg-escape-identifier pgmacs--table)
                        (pg-escape-identifier colname))))
      (when (y-or-n-p (format "Really run SQL '%s'?" sql))
        (let ((res (pg-exec pgmacs--con sql)))
          (pgmacs--notify "%s" (pg-result res :status)))))))

(defun pgmacs--run-analyze (&rest _ignore)
  "Run ANALYZE on the current PostgreSQL table."
  (let* ((sql (format "ANALYZE %s" (pg-escape-identifier pgmacs--table)))
         (res (pg-exec pgmacs--con sql)))
    (pgmacs--notify "%s" (pg-result res :status))))


(defun pgmacs--paginated-next (&rest _ignore)
  "Move to the next page of the paginated PostgreSQL table."
  (interactive)
  (cl-incf pgmacs--offset pgmacs-row-limit)
  (pgmacs--display-table pgmacs--table))

(defun pgmacs--paginated-prev (&rest _ignore)
  "Move to the previous page of the paginated PostgreSQL table."
  (interactive)
  (cl-decf pgmacs--offset pgmacs-row-limit)
  (pgmacs--display-table pgmacs--table))


(defun pgmacs--row-list-help (&rest _ignore)
  "Open a buffer describing keybindings in a row-list buffer."
  (interactive)
  (pop-to-buffer "*PGmacs row-list help*")
  (erase-buffer)
  (help-mode)
  (cl-flet ((shw (key msg)
              (insert (propertize (format "%12s" key) 'face '(:foreground "blue")))
              (insert (propertize " → " 'face '(:foreground "gray")))
              (insert msg "\n")))
    (let ((inhibit-read-only t))
      (shw "RET" "Edit the value at point in the minibuffer")
      (shw "w" "Edit the value at point in a widget-based buffer")
      (shw "<backspace>" "Delete the row at point")
      (shw "DEL" "Delete the row at point")
      (shw "+" "Insert a new row, prompting for new values in minibuffer")
      (shw "i" "Insert a new row, prompting for new values in a dedicated buffer")
      (shw "k" "Copy the row at point")
      (shw "y" "Yank the previously copied row and insert into the table")
      (shw "j" "Copy the current row to the kill-ring in JSON format")
      (shw "n" "Next page of output (if table contents are paginated)")
      (shw "p" "Previous page of output (if table contents are paginated)")
      (shw "e" "New buffer with output from SQL query")
      (shw "<" "Go to the first table in the table list")
      (shw ">" "Go to the last table in the table list")
      (shw "{" "Shrink the horizontal space used by the current column")
      (shw "}" "Grow the horizontal space used by the current column")
      (shw "r" "Redraw the table (does not refetch data from PostgreSQL)")
      (shw "q" "Bury this buffer")
      (shrink-window-if-larger-than-buffer)
      (goto-char (point-min)))))


;; TODO: add additional information as per psql
;; Table « public.books »
;; Colonne |           Type           | Collationnement | NULL-able |            Par défaut
;; ---------+--------------------------+-----------------+-----------+-----------------------------------
;; id      | integer                  |                 | not null  | nextval('books_id_seq'::regclass)
;; title   | text                     |                 |           |
;; price   | numeric                  |                 |           |
;; created | timestamp with time zone |                 | not null  | now()
;; Index :
;; "books_pkey" PRIMARY KEY, btree (id)
;; Contraintes de vérification :
;; "check_price_gt_zero" CHECK (price >= 0::numeric)
;; Référencé par :
;; TABLE "book_author" CONSTRAINT "book_author_book_id_fkey" FOREIGN KEY (book_id) REFERENCES books(id)

(defun pgmacs--display-table (table)
  "Create and populate a buffer to display PostgreSQL table TABLE.
Table may be specified as a string or as a schema-qualified pg-qualified-name
object."
  (let* ((con pgmacs--con)
         (t-id (pg-escape-identifier table))
         (t-pretty (pgmacs--display-identifier table)))
    (pop-to-buffer-same-window (format "*PostgreSQL %s %s*" (pgcon-dbname con) t-pretty))
    (pgmacs--start-progress-reporter "Retrieving data from PostgreSQL")
    (pgmacs-mode)
    ;; Place some initial content in the buffer early up.
    (let ((inhibit-read-only t)
          (owner (pg-table-owner con table)))
      (erase-buffer)
      (insert (propertize (format "PostgreSQL table %s, owned by %s\n" t-pretty owner) 'face 'bold)))
    (let* ((primary-keys (pgmacs--table-primary-keys con table))
           (comment (pg-table-comment con table))
           (offset (or pgmacs--offset 0))
           (portal (format "pgbp%s" t-pretty))
           (sql (format "SELECT * FROM %s OFFSET %s" t-id offset))
           (res (pg-exec-prepared con sql (list) :max-rows pgmacs-row-limit :portal portal))
           (rows (pg-result res :tuples))
           (column-names (mapcar #'cl-first (pg-result res :attributes)))
           (column-type-oids (mapcar #'cl-second (pg-result res :attributes)))
           (column-type-names (mapcar #'pg--lookup-type-name column-type-oids))
           (column-alignment (mapcar #'pgmacs--alignment-for column-type-names))
           (column-meta (mapcar (lambda (col) (pgmacs--column-info con table col)) column-names))
           (column-formatters (mapcar #'pgmacs--value-formatter column-type-names))
           (value-widths (mapcar #'pgmacs--value-width column-type-names))
           (column-widths (cl-loop for w in value-widths
                                   for name in column-names
                                   collect (1+ (max w (length name)))))
           (columns (cl-loop
                     for name in column-names
                     for meta in column-meta
                     for align in column-alignment
                     for fmt in column-formatters
                     for w in column-widths
                     collect (make-pgmacstbl-column
                              :name (propertize name
                                                'face 'pgmacs-table-header
                                                'help-echo meta)
                              :align align
                              :min-width (1+ (max w (length name)))
                              :formatter fmt
                              :displayer (pgmacs--make-column-displayer meta))))
           (inhibit-read-only t)
           (pgmacstbl (make-pgmacstbl
                    :insert nil
                    :use-header-line nil
                    :columns columns
                    :row-colors pgmacs-row-colors
                    :objects rows
                    ;; same syntax for keys as keymap-set
                    :actions `("RET" (lambda (row) (pgmacs--edit-value-minibuffer row ',primary-keys))
                               "w" (lambda (row) (pgmacs--edit-value-widget row ',primary-keys))
                               "<delete>" (lambda (row) (pgmacs--delete-row row ',primary-keys))
                               "<deletechar>" (lambda (row) (pgmacs--delete-row row ',primary-keys))
                               "<backspace>" (lambda (row) (pgmacs--delete-row row ',primary-keys))
                               "DEL" (lambda (row) (pgmacs--delete-row row ',primary-keys))
                               "h" pgmacs--row-list-help
                               "+" pgmacs--insert-row
                               "i" pgmacs--insert-row-widget
                               "k" pgmacs--copy-row
                               "y" pgmacs--yank-row
                               "e" pgmacs-run-sql
                               "r" pgmacs--redraw-pgmacstbl
                               "j" pgmacs--row-as-json
                               ;; "n" and "p" are bound when table is paginated to next/prev page
                               "<" (lambda (&rest _ignored)
                                     (text-property-search-backward 'pgmacstbl)
                                     (next-line))
                               ">" (lambda (&rest _ignored)
                                     (text-property-search-forward 'pgmacstbl)
                                     (previous-line))
                               "q" (lambda (&rest ignore) (kill-buffer))))))
      (setq-local pgmacs--con con
                  pgmacs--table table
                  pgmacs--offset offset
                  pgmacs--column-type-names (apply #'vector column-type-names)
                  buffer-read-only t
                  truncate-lines t)
      (when comment
        (insert (propertize "Comment" 'face 'bold))
        (insert (format ": %s\n" comment)))
      (let* ((sql "SELECT pg_size_pretty(pg_total_relation_size($1)),
                          pg_size_pretty(pg_indexes_size($1))")
             (res (pg-exec-prepared con sql `((,t-id . "text"))))
             (row (pg-result res  :tuple 0)))
        (insert (propertize "On-disk-size" 'face 'bold))
        (insert (format ": %s" (cl-first row)))
        (insert (format " (indexes %s)\n" (cl-second row))))
      (insert (propertize "Columns" 'face 'bold))
      (insert ":\n")
      (let ((colinfo (list)))
        (dolist (col column-names)
          (push (format "%s: %s" col (pgmacs--column-info con table col)) colinfo))
        (setq colinfo (reverse colinfo))
        (let ((last (pop colinfo)))
          (dolist (c colinfo)
            (insert "├ ")
            (insert c)
            (insert "\n"))
          (insert "└ ")
          (insert last)
          (insert "\n")))
      (insert "\n")
      (insert-text-button "Export table to CSV buffer"
                          'action #'pgmacs--table-to-csv
                          'help-echo "Export this table to a CSV buffer")
      (unless primary-keys
        (insert "   ")
        (insert-text-button "Add primary key to table"
                            'action #'pgmacs--add-primary-key
                            'help-echo "Add a PRIMARY KEY to enable editing"))
      (insert "  ")
      (insert-text-button "ANALYZE this table"
                          'action #'pgmacs--run-analyze
                          'help-echo "Run ANALYZE on this table")
      (insert "\n\n")
      (when (pg-result res :incomplete)
        (pgmacs-paginated-mode)
        (when (> pgmacs--offset pgmacs-row-limit)
          (insert-text-button
           (format "Prev. %s rows" pgmacs-row-limit)
           'action #'pgmacs--paginated-prev)
          (insert "   "))
        (insert-text-button
         (format "Next %s rows" pgmacs-row-limit)
         'action #'pgmacs--paginated-next)
        (insert "\n\n"))
      (if (null rows)
          (insert "(no rows in table)")
        (pgmacstbl-insert pgmacstbl))
      (pgmacs--stop-progress-reporter))))


;; This is similar to pgmacstbl-revert, but works correctly with a buffer than contains content other
;; than the pgmacstbl.
(defun pgmacs--redraw-pgmacstbl (&rest _ignore)
  "Redraw the pgmacstbl in the current buffer."
  (let ((pgmacstbl (pgmacstbl-current-table))
        (object (pgmacstbl-current-object))
        (column (pgmacstbl-current-column))
        (inhibit-read-only t)
        (start (save-excursion
                 (goto-char (point-max))
                 (text-property-search-backward 'pgmacstbl)
                 (point)))
        (end (save-excursion
               (goto-char (point-min))
               (text-property-search-forward 'pgmacstbl)
               (point))))
    (unless pgmacstbl
      (user-error "No table under point"))
    (delete-region start end)
    (pgmacstbl-insert pgmacstbl)
    (when object
      (pgmacstbl-goto-object object))
    (when column
      (pgmacstbl-goto-column column))))

(defun pgmacs--display-backend-information (&rest _ignore)
  "Create a buffer with information concerning the current PostgreSQL backend."
  (let ((con pgmacs--con))
    (pop-to-buffer (get-buffer-create "*PostgreSQL backend information*"))
    (pgmacs-transient-mode)
    (let* ((res (pg-exec con "SELECT inet_server_addr(), inet_server_port(), pg_backend_pid()"))
           (row (pg-result res :tuple 0))
           (addr (cl-first row))
           (port (cl-second row))
           (pid (cl-third row)))
      (insert (format "Connected to backend with pid %s" pid))
      (if addr
          (insert (format " at %s:%s\n" addr port))
        (insert " over Unix-domain socket\n")))
    (let* ((res (pg-exec con "SELECT current_user, current_setting('is_superuser')"))
           (row (pg-result res :tuple 0)))
      (insert (format "Connected as user %s (%ssuperuser)\n"
                      (cl-first row)
                      (if (cl-second row) "" "not "))))
    (let* ((res (pg-exec con "SELECT current_setting('in_hot_standby')"))
           (row (pg-result res :tuple 0)))
      (insert (apply #'format "In hot standby: %s\n" row)))
    (let* ((res (pg-exec con "SELECT pg_postmaster_start_time()"))
           (dtime (car (pg-result res :tuple 0)))
           (fmt (funcall (pgmacs--value-formatter "timestamp") dtime)))
      (insert (format "PostgreSQL running since %s\n" fmt)))
    (let* ((res (pg-exec con "SELECT current_setting('client_encoding')"))
           (row (pg-result res :tuple 0)))
      (insert (apply #'format "Client encoding: %s\n" row)))
    (let* ((res (pg-exec con "SELECT current_setting('server_encoding')"))
           (row (pg-result res :tuple 0)))
      (insert (apply #'format "Server encoding: %s\n" row)))
    (let* ((res (pg-exec con "SELECT current_setting('TimeZone')"))
           (row (pg-result res :tuple 0)))
      (insert (apply #'format "Server timezone: %s\n" row)))
    (let* ((res (pg-exec con "SELECT pg_listening_channels()"))
           (channels (pg-result res :tuples)))
      (when channels
        (insert "Asynchronous notification channels for the current session:\n")
        (dolist (ch channels)
          (insert "  " ch "\n"))))
    (let* ((res (pg-exec con "SELECT name, default_version, installed_version FROM pg_available_extensions"))
           (exts (pg-result res :tuples)))
      (insert "PostgreSQL extensions:")
      (if exts (insert "\n") (insert " (none)\n"))
      (when exts
        (push (list "Name" "Default version" "Installed version") exts))
      ;; TODO: could include a "Install" button for uninstalled extensions
      (dolist (ext exts)
        (insert (apply #'format "%20s %17s %18s\n" ext))))
    (shrink-window-if-larger-than-buffer)
    (goto-char (point-min))))


(defvar pgmacs--stat-activity-columns
  (list "datname" "usename" "client_addr" "backend_start" "xact_start" "query_start" "wait_event"))

(defun pgmacs--display-stat-activity (&rest _ignore)
  "Display information from PostgreSQL's pg_stat_activity table."
  (let* ((cols (string-join pgmacs--stat-activity-columns ","))
         (sql (format "SELECT %s FROM pg_stat_activity" cols)))
    (pgmacs-show-result pgmacs--con sql)))

;; We can't make this interactive because it's called from the keymap on a table list, where we
;; receive unnecessary arguments related to the current cursor position.
;;
;; TODO: allow input from a buffer which is set to sql-mode.
(defun pgmacs-run-sql (&rest _ignore)
  "Prompt for an SQL query and display the output in a dedicated buffer."
  (interactive)
  (let ((sql (read-from-minibuffer "SQL query: ")))
    (pgmacs-show-result pgmacs--con sql)))


(defun pgmacs-show-result (con sql)
  "Create a buffer to show the results of PostgreSQL query SQL.
Uses PostgreSQL connection CON."
  (pop-to-buffer (get-buffer-create "*PostgreSQL TMP*"))
  (pgmacs-mode)
  (setq-local pgmacs--con con
              truncate-lines t)
  (pgmacs--start-progress-reporter "Retrieving data from PostgreSQL")
  ;; Insert initial content into buffer early.
  (let ((inhibit-read-only t))
    (erase-buffer)
    (remove-overlays)
    (insert (propertize "PostgreSQL query output" 'face 'bold))
    (insert "\n")
    (insert (propertize "SQL" 'face 'bold))
    (insert (format ": %s\n\n" sql)))
  (let* ((res (pg-exec con sql))
         (rows (pg-result res :tuples)))
    (cond ((null rows)
           (insert "(no rows)"))
          (t
           (let* ((column-names (mapcar #'cl-first (pg-result res :attributes)))
                  (column-type-oids (mapcar #'cl-second (pg-result res :attributes)))
                  (column-type-names (mapcar #'pg--lookup-type-name column-type-oids))
                  (column-formatters (mapcar #'pgmacs--value-formatter column-type-names))
                  (value-widths (mapcar #'pgmacs--value-width column-type-names))
                  (column-widths (cl-loop for w in value-widths
                                          for name in column-names
                                          collect (1+ (max w (length name)))))
                  (columns (cl-loop for name in column-names
                                    for fmt in column-formatters
                                    for w in column-widths
                                    collect (make-pgmacstbl-column
                                             :name (propertize name 'face 'pgmacs-table-header)
                                             :min-width (1+ (max w (length name)))
                                             :formatter fmt)))
                  (inhibit-read-only t)
                  (pgmacstbl (make-pgmacstbl
                              :insert nil
                              :use-header-line nil
                              :face 'pgmacs-table-data
                              :columns columns
                              :row-colors pgmacs-row-colors
                              :objects rows
                              :actions '("e" pgmacs-run-sql
                                         "q" (lambda (&rest _ignore) (kill-buffer))))))
             (pgmacstbl-insert pgmacstbl))))
    (shrink-window-if-larger-than-buffer)
    (pgmacs--stop-progress-reporter)))


;; If the cursor is on the Comment column, allow the user to set the table comment. Otherwise,
;; display the table in a separate buffer.
(defun pgmacs--table-list-RET (table-row)
  "Called on RET on a line in the list-of-tables buffer TABLE-ROW."
  (let* ((tbl (pgmacstbl-current-table))
         (col-id (pgmacstbl-current-column))
         (col (nth col-id (pgmacstbl-columns tbl)))
         (col-name (pgmacstbl-column-name col)))
    (cond ((string= "Comment" col-name)
           (let ((comment (read-from-minibuffer "New table comment: "))
                 (new-row (copy-sequence table-row)))
             (setf (pg-table-comment pgmacs--con (car table-row)) comment)
             (setf (nth col-id new-row) comment)
             ;; pgmacstbl-update-object doesn't work, so insert then delete old row
             (pgmacstbl-insert-object tbl new-row table-row)
             (pgmacstbl-remove-object tbl table-row)
             (pgmacs--redraw-pgmacstbl)))
          ;; TODO perhaps change owner (if we are superuser)
          (t
           (pgmacs--display-table (car table-row))))))

(defun pgmacs--table-list-delete (table-row)
  "Delete (drop) the PostgreSQL table specified by TABLE-ROW."
  (let* ((tbl (pgmacstbl-current-table))
         (table (car table-row))
         (t-id (pg-escape-identifier table)))
    (when (yes-or-no-p (format "Really drop PostgreSQL table %s? " t-id))
      ;; We can't use a prepared statement for this dynamic SQL statement
      (let* ((sql (format "DROP TABLE %s" t-id))
             (res (pg-exec pgmacs--con sql)))
        (pgmacs--notify "%s" (pg-result res :status))
        (pgmacstbl-remove-object tbl table-row)
        (pgmacs--redraw-pgmacstbl)))))

(defun pgmacs--table-list-rename (table-row)
  "Rename the PostgreSQL table specified by TABLE-ROW."
  (let* ((table (car table-row))
         (t-id (pg-escape-identifier table))
         (new (read-string (format "Rename table %s to: " t-id)))
         (new-id (pg-escape-identifier new)))
    (let* ((sql (format "ALTER TABLE %s RENAME TO %s" t-id new-id))
           (res (pg-exec pgmacs--con sql)))
      (pgmacs--notify "%s" (pg-result res :status))
      ;; Redraw in the table-list buffer.
      (setf (cl-first table-row) new)
      (pgmacs--redraw-pgmacstbl))))

(defun pgmacs--table-list-redraw (&rest _ignore)
  "Refresh the PostgreSQL table-list buffer."
  (interactive)
  (let ((con pgmacs--con))
    (kill-buffer)
    (pgmacs-open con)))


(defun pgmacs--table-list-help (&rest _ignore)
  "Show keybindings active in a table-list buffer."
  (interactive)
  (pop-to-buffer "*PGmacs table-list help*")
  (erase-buffer)
  (help-mode)
  (cl-flet ((shw (key msg)
              (insert (propertize (format "%12s" key) 'face '(:foreground "blue")))
              (insert (propertize " → " 'face '(:foreground "gray")))
              (insert msg "\n")))
    (let ((inhibit-read-only t))
      (shw "RET" "Open a new buffer to browse/edit the table at point")
      (shw "<deletechar>" "Delete the table at point")
      (shw "r" "Rename the table at point")
      (shw "e" "New buffer with output from SQL query")
      (shw "<" "Go to the first table in the table list")
      (shw ">" "Go to the last table in the table list")
      (shw "{" "Shrink the horizontal space used by the current column")
      (shw "}" "Grow the horizontal space used by the current column")
      (shw "q" "Bury this buffer")
      (shrink-window-if-larger-than-buffer)
      (goto-char (point-min)))))

;;;###autoload
(defun pgmacs-open (con)
  "Browse the contents of PostgreSQL database to which we are connected over CON."
  ;; (pg-enable-query-log con)
  (pg-hstore-setup con)
  (pop-to-buffer-same-window (format "*PostgreSQL %s*" (pgcon-dbname con)))
  (pgmacs-mode)
  (setq-local pgmacs--con con
              buffer-read-only t
              truncate-lines t)
  (pgmacs--start-progress-reporter "Retrieving PostgreSQL table list")
  (set-process-query-on-exit-flag (pgcon-process con) nil)
  ;; Make sure some initial content is visible in the buffer, in case of a slow connection to
  ;; PostgreSQL.
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (pg-backend-version con)))
  (let* ((dbname (pgcon-dbname con))
         (inhibit-read-only t)
         (pgmacstbl (make-pgmacstbl
                  :insert nil
                  :use-header-line nil
                  :columns (list
                            (make-pgmacstbl-column
                             :name (propertize "Table" 'face 'pgmacs-table-header)
                             :width 20
                             :primary t
                             :align 'right)
                            (make-pgmacstbl-column
                             :name (propertize "Rows" 'face 'pgmacs-table-header)
                             :width 7 :align 'right)
                            (make-pgmacstbl-column
                             :name (propertize "Size on disk" 'face 'pgmacs-table-header)
                             :width 13 :align 'right)
                            (make-pgmacstbl-column
                             :name (propertize "Owner" 'face 'pgmacs-table-header)
                             :width 13 :align 'right)
                            (make-pgmacstbl-column
                             :name (propertize "Comment" 'face 'pgmacs-table-header)
                             :width 30 :align 'left))
                  :row-colors pgmacs-row-colors
                  :face 'pgmacs-table-data
                  ;; :column-colors '("#202020" "#404040")
                  :objects (pgmacs--list-tables)
                  :actions '("h" pgmacs--table-list-help
                             "RET" pgmacs--table-list-RET
                             "<deletechar>" pgmacs--table-list-delete
                             "r" pgmacs--table-list-rename
                             "g" pgmacs--table-list-redraw
                             "e" pgmacs-run-sql
                             ;; the functions pgmacstbl-beginning-of-table and pgmacstbl-end-of-table don't work when
                             ;; we have inserted text before the pgmacstbl.
                             "<" (lambda (&rest _ignored)
                                   (text-property-search-backward 'pgmacstbl)
                                   (next-line))
                             ">" (lambda (&rest _ignored)
                                   (text-property-search-forward 'pgmacstbl)
                                   (previous-line))
                             "q"  (lambda (&rest _ignored) (kill-buffer)))
                  :getter (lambda (object column pgmacstbl)
                            (pcase (pgmacstbl-column pgmacstbl column)
                              ("Table" (pgmacs--display-identifier (cl-first object)))
                              ("Rows" (cl-second object))
                              ("Size on disk" (cl-third object))
                              ("Owner" (cl-fourth object))
                              ("Comment" (cl-fifth object)))))))
    (let* ((res (pg-exec con "SELECT current_user, pg_backend_pid(), pg_is_in_recovery()"))
           (row (pg-result res :tuple 0)))
      (insert (format "\nConnected to database %s as user %s (pid %d %s)\n"
                      dbname
                      (cl-first row)
                      (cl-second row)
                      (if (cl-third row) "RECOVERING" "PRIMARY"))))
    (let* ((sql "SELECT pg_size_pretty(pg_database_size($1))")
           (res (pg-exec-prepared con sql `((,dbname . "text"))))
           (size (cl-first (pg-result res :tuple 0))))
      (insert (format "Total database size: %s\n" size)))
    ;; Perhaps also display output from
    ;; select state, count(*) from pg_stat_activity where pid <> pg_backend_pid() group by 1 order by 1;'
    ;; see https://gitlab.com/postgres-ai/postgresql-consulting/postgres-howtos/-/blob/main/0068_psql_shortcuts.md
    (insert "\n")
    (insert-text-button "More backend information"
                        'action #'pgmacs--display-backend-information)
    (insert "   ")
    (insert-text-button "Stat activity"
                        'action #'pgmacs--display-stat-activity
                        'help-echo "Show information from the pg_stat_activity table")
    (insert "   ")
    (insert-text-button
     "Replication stats"
     'action (lambda (&rest _ignore)
               ;; FIXME probably only want a subset of these columns
               (pgmacs-show-result con "SELECT * FROM pg_stat_replication"))
     'help-echo "Show information on PostgreSQL replication status")
    (insert "\n\n")
    (pgmacstbl-insert pgmacstbl)
    (pgmacs--stop-progress-reporter)))


;;;###autoload
(defun pgmacs-open-string (connection-string)
  "Open PGmacs on database specified by CONNECTION-STRING.
CONNECTION-STRING is a PostgreSQL connection string of the form
`dbname=mydb user=me host=localhost password=foo'.
The supported keywords in the connection string are host,
hostaddr, port, dbname, user, password, sslmode (partial support)
and application_name."
  (interactive "sPostgreSQL connection string: ")
  (pgmacs--start-progress-reporter "Connecting to PostgreSQL")
  (pgmacs-open (pg-connect/string connection-string)))

;;;###autoload
(defun pgmacs-open-uri (connection-uri)
  "Open PGmacs on database specified by CONNECTION-URI.
CONNECTION-URI is a PostgreSQL connection URI of the form
`postgresql://user:pass@host/dbname'."
  (interactive "sPostgreSQL connection URI: ")
  (pgmacs--start-progress-reporter "Connecting to PostgreSQL")
  (pgmacs-open (pg-connect/uri connection-uri)))

;;;###autoload
(defun pgmacs ()
  "Open a widget-based login buffer for PostgreSQL."
  (interactive)
  (switch-to-buffer "*PGmacs connection widget*")
  (kill-all-local-variables)
  (remove-overlays)
  (widget-insert (propertize "Connect to PostgreSQL database" 'face 'bold))
  (widget-insert "\n\n")
  (let* ((w-dbname
         (progn
           (widget-insert (format "%18s: " "Database name"))
           (widget-create 'editable-field
                          :size 20)))
        (w-hostname
         (progn
           (widget-insert (format "\n%18s: " "Hostname"))
           (widget-create 'editable-field
                          :help-echo "The host where PostgreSQL is running"
                          :default ""
                          :size 20)))
        (w-port
         (progn
           (widget-insert (format "\n%18s: " "Port"))
           (widget-create 'natnum
                          :format "%v"
                          :size 20
                          "5432")))
        (w-username
         (progn
           (widget-insert (format "\n%18s: " "Username"))
           (widget-create 'editable-field
                          :help-echo "Authenticate as this user"
                          :size 20)))
        (w-password
         (progn
           (widget-insert (format "\n%18s: " "Password"))
           (widget-create 'editable-field
                          :secret ?*
                          :size 20)))
        (w-tls
         (progn
           (widget-insert (format "\n%18s: " "TLS encryption"))
           (widget-create 'checkbox
                          :help-echo "Whether to use an encrypted connection"))))
    (widget-insert "\n\n")
    (widget-insert (propertize "Tab next ∣  Shift-Tab prev"
                               'face 'shadow))
    (widget-insert "\n\n")
    (widget-create 'push-button
                   :notify (lambda (&rest _ignore)
                             (setq pgmacs--progress
                                   (make-progress-reporter "Connecting to PostgreSQL"))
                             (let ((con (pg-connect (widget-value w-dbname)
                                                    (widget-value w-username)
                                                    (widget-value w-password)
                                                    (widget-value w-hostname)
                                                    (widget-value w-port)
                                                    (widget-value w-tls))))
                               (pgmacs-open con)))
                   "Connect")
    (widget-insert "\n")
    (use-local-map widget-keymap)
    (widget-setup)
    (goto-char (point-min))
    (widget-forward 1)))


(provide 'pgmacs)

;;; pgmacs.el ends here
