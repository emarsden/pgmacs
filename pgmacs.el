;;; pgmacs.el --- Emacs is editing a PostgreSQL database  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Eric Marsden
;; Author: Eric Marsden <eric.marsden@risk-engineering.org>
;; Version: 0.3
;; Package-Requires: ((emacs "29.1") (pg "0.31"))
;; URL: https://github.com/emarsden/pgmacs/
;; Keywords: data, PostgreSQL, database
;; SPDX-License-Identifier: GPL-2.0-or-later

;;; Commentary:
;;
;; See README.md at https://github.com/emarsden/pgmacs/


;;; Code:

(require 'cl-lib)
(require 'vtable)                       ; note: requires Emacs 29
(require 'button)
(require 'widget)
(require 'pg)


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
If more rows are present in the PostgreSQL query result, the display of results will be
paginated. You may wish to set this to a low value if accessing PostgreSQL over a slow
network link."
  :type 'number
  :group 'pgmacs)

(defcustom pgmacs-mode-hook nil
  "Mode hook for `pgmacs-mode'."
  :type 'hook
  :group 'pgmacs)

(defvar pgmacs-mode-map (make-sparse-keymap))

(keymap-set pgmacs-mode-map (kbd "q") 'bury-buffer)
(keymap-set pgmacs-mode-map (kbd "e") (lambda (&rest _ignored) (pgmacs-run-sql)))

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
  (setq pgmacs--progress (make-progress-reporter msg))
  (setq pgmacs--progress-timer
        (run-with-timer 0.2 0.2 (lambda () (progress-reporter-update pgmacs--progress)))))

(defun pgmacs--update-progress (msg)
  (when pgmacs--progress
    (progress-reporter-update msg)))

(defun pgmacs--stop-progress-reporter ()
  (when pgmacs--progress
    (progress-reporter-done pgmacs--progress)
    (setq pgmacs--progress nil))
  (when pgmacs--progress-timer
    (cancel-timer pgmacs--progress-timer)
    (setq pgmacs--progress-timer nil)))


;; Used for copying and pasting rows
(defvar-local pgmacs--kill-ring nil)


(defvar-local pgmacs--con nil)
(defvar-local pgmacs--table nil)
(defvar-local pgmacs--column-type-names nil)
(defvar-local pgmacs--offset nil)


(defun pgmacs--notify (fmt &rest args)
  (message (concat "PostgreSQL> " (apply #'format (cons fmt args)))))

(defun pgmacs--value-formatter (type-name)
  (cond ((or (string= type-name "timestamp")
             (string= type-name "timestamptz")
	     (string= type-name "date"))
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
             (maphash (lambda (k v) (push (format "%s=>%s" k v) items)) ht)
             (string-join items ","))))
        ((string= type-name "json")
         #'json-serialize)
        (t
         (lambda (val) (format "%s" val)))))

;; How wide should we make a column containing elements of this type?
(defun pgmacs--value-width (type-name)
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
        ((string= type-name "date") 18)
        ((string= type-name "float8") 10)
        ((string= type-name "float8") 10)
        ((string= type-name "timestamp") 20)
        ((string= type-name "timestamptz") 20)
        ((string= type-name "datetime") 20)
        ((string= type-name "time") 12)
        ((string= type-name "reltime") 10)
        ((string= type-name "timespan") 12)
        (t 10)))

(defun pgmacs--alignment-for (type-name)
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
  (unless (json-available-p)
    (error "Emacs is not compiled with JSON support"))
  (let* ((vtable (vtable-current-table))
         (cols (vtable-columns vtable))
         (ht (make-hash-table :test #'equal)))
    (cl-loop
     for col in cols
     for v in current-row
     do (puthash (vtable-column-name col) v ht))
    (kill-new (json-encode ht))
    (message "JSON copied to kill ring")))

(defun pgmacs--read-value/minibuffer (name type prompt current-value)
  (let* ((prompt (format prompt name type)))
    (read-string prompt (format "%s" current-value))))

;; TODO: perhaps if the field value is very long or of BYTEA type, prompt "really want to edit in
;; minibuffer" and suggest using the widget editing mode instead.
(defun pgmacs--read-value (name type prompt current-value)
  (let* ((user-provided (pgmacs--read-value/minibuffer name type prompt current-value))
         (parser (pg-lookup-parser type))
         (ce (pgcon-client-encoding pgmacs--con)))
    (if parser (funcall parser user-provided ce) user-provided)))

;; Edit the column value at point by prompting for the new value in the minibuffer.
(defun pgmacs--edit-value/minibuffer (row primary-keys)
  (if (null primary-keys)
      (warn "Can't edit content of a table that has no PRIMARY KEY")
    (let* ((vtable (or (vtable-current-table)
                       (error "Cursor is not in a current vtable")))
           (current-row (or (vtable-current-object)
                            (error "Cursor is not on a vtable row")))
           (cols (vtable-columns vtable))
           (col-id (or (vtable-current-column)
                       (error "Not on a vtable column")))
           (col (nth col-id cols))
           (col-name (vtable-column-name col))
           (col-type (aref pgmacs--column-type-names col-id))
           (pk (cl-first primary-keys))
           (pk-col-id (or (cl-position pk cols :key #'vtable-column-name :test #'string=)
                          (error "Can't find primary key %s in the vtable column list" pk)))
           (pk-col-type (and pk-col-id (aref pgmacs--column-type-names pk-col-id)))
           (pk-value (and pk-col-id (nth pk-col-id row))))
      (unless pk-value
        (error "Can't find value for primary key %s" pk))
      (let* ((current (nth col-id current-row))
             (new-value (pgmacs--read-value (substring-no-properties col-name)
                                            (substring-no-properties col-type)
                                            "Change %s (%s) to: "
                                            (substring-no-properties current)))
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
          ;; vtable-update-object doesn't work, so insert then delete old row
          (vtable-insert-object vtable new-row current-row)
          (vtable-remove-object vtable current-row))))))

(defun pgmacs--widget-for (_type current-value)
  ;; TODO improve this choice based on type
  (widget-create 'editable-field
                 :size 40
                 (format "%s" current-value)))

(defun pgmacs--edit-value/widget (row primary-keys)
  "Edit the current column value in ROW in a dedicated widget buffer."
  (if (null primary-keys)
      (warn "Can't edit content of a table that has no PRIMARY KEY")
    (let* ((con pgmacs--con)
           (table pgmacs--table)
           (vtable (vtable-current-table))
           (current-row (vtable-current-object))
           (cols (vtable-columns vtable))
           (col-id (vtable-current-column))
           (col (nth col-id cols))
           (col-name (vtable-column-name col))
           (col-type (aref pgmacs--column-type-names col-id))
           (pk (cl-first primary-keys))
           (pk-col-id (cl-position pk cols :key #'vtable-column-name :test #'string=))
           (pk-col-type (aref pgmacs--column-type-names pk-col-id))
           (pk-value (and pk-col-id (nth pk-col-id row))))
      (unless pk-value
        (error "Can't find value for primary key %s" pk))
      (let* ((current (nth col-id current-row))
             (sql (format "UPDATE %s SET %s = $1 WHERE %s = $2"
                          (pg-escape-identifier pgmacs--table)
                          (pg-escape-identifier col-name)
                          (pg-escape-identifier pk)))
             (updater (lambda (user-provided)
                        (let* ((parser (pg-lookup-parser col-type))
                               (ce (pgcon-client-encoding pgmacs--con))
                               (new-value (if parser (funcall parser user-provided ce)
                                            user-provided))
                               (res (pg-exec-prepared pgmacs--con sql
                                                      `((,new-value . ,col-type)
                                                        (,pk-value . ,pk-col-type)))))
                          (pgmacs--notify "%s" (pg-result res :status))
                          (let ((new-row (copy-sequence current-row)))
                            (setf (nth col-id new-row) new-value)
                            ;; vtable-update-object doesn't work, so insert then delete old row
                            (vtable-insert-object vtable new-row current-row)
                            (vtable-remove-object vtable current-row))))))
        (switch-to-buffer "*PGmacs update widget*")
        (erase-buffer)
        (remove-overlays)
        (kill-all-local-variables)
        (setq-local pgmacs--con con
                    pgmacs--table table)
        (widget-insert (propertize (format "Update column %s" col-name) 'face 'bold))
        (widget-insert "\n\n")
        (widget-insert (format "Change %s (type %s) to:" col-name col-type))
        (widget-insert "\n\n")
        (let* ((w-updated
                (progn
                  (widget-insert (format "%12s: " "New value"))
                  (pgmacs--widget-for col-type current))))
          (widget-insert "\n\n")
          (widget-create 'push-button
                         :notify (lambda (&rest _ignore)
                                   (funcall updater (widget-value w-updated))
                                   (kill-buffer (current-buffer)))
                         "Update")
          (widget-insert "\n")
          (use-local-map widget-keymap)
          (widget-setup)
          (goto-char (point-min))
          (widget-forward 1))))))

(defun pgmacs--delete-row (row primary-keys)
  "Delete ROW from the current table."
  (if (null primary-keys)
      (warn "Can't edit content of a table that has no PRIMARY KEY")
    (when (y-or-n-p (format "Really delete PostgreSQL row %s?" row))
      (let* ((vtable (vtable-current-table))
             (cols (vtable-columns vtable))
             (pk (cl-first primary-keys))
             (pk-col-id (cl-position pk cols :key #'vtable-column-name :test #'string=))
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
        (vtable-remove-object vtable row)))))

(defun pgmacs--insert-row (current-row)
  "Insert a new row of data into the current table after CURRENT-ROW.
Uses the minibuffer to prompt for new values."
  ;; TODO we need to handle the case where there is no existing vtable because the underlying SQL
  ;; table is empty.
  (let* ((vtable (vtable-current-table))
         (cols (vtable-columns vtable))
         (col-names (list))
         (values (list))
         (value-types (list)))
    (dolist (col cols)
      (let* ((col-name (vtable-column-name col))
             (col-id (cl-position col-name cols :key #'vtable-column-name :test #'string=))
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
      ;; It's tempting to use vtable-insert-object here to avoid a full refresh of the vtable.
      ;; However, we don't know what values were chosen for any columns that have a default.
      (pgmacs--display-table pgmacs--table))))

(defun pgmacs--insert-row/widget (current-row)
  "Insert a new row of data into the current table after CURRENT-ROW.
Uses a widget-based buffer to prompt for new values."
  (let* ((con pgmacs--con)
         (table pgmacs--table)
         (ce (pgcon-client-encoding pgmacs--con))
         (vtable (vtable-current-table))
         (cols (vtable-columns vtable))
         (editable-cols (list))
         (col-names (list))
         (col-types (list)))
    ;; Determine which of the columns are editable (those that do not have a defined default)
    (dolist (col cols)
      (let* ((col-name (vtable-column-name col))
             (col-id (cl-position col-name cols :key #'vtable-column-name :test #'string=))
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
                        ;; It's tempting to use vtable-insert-object here to avoid a full refresh of
                        ;; the vtable. However, we don't know what values were chosen for any columns
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
          (widget-insert (format "\n%12s: " name))
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
  "Copy current row to our internal kill-ring."
  (setq pgmacs--kill-ring (cons pgmacs--table current-row))
  (message "Row copied to PGmacs kill ring"))

;; Insert new row at current position based on content of our "kill ring".
(defun pgmacs--yank-row (_current-row)
  "Insert a new row into the current table after the current row, based on the last copied row."
  (unless pgmacs--kill-ring
    (error "PGmacs kill ring is empty"))
  (unless (eq (car pgmacs--kill-ring) pgmacs--table)
    (error "Can't paste into a different PostgreSQL table"))
  (message "Pasting row from PGmacs kill ring")
  ;; Insert a new row based on the copied row, but without specifying values for the columns that
  ;; have a default value
  (let* ((yanked-row (cdr pgmacs--kill-ring))
         (vtable (vtable-current-table))
         (cols (vtable-columns vtable))
         (col-names (list))
         (values (list))
         (value-types (list)))
    (cl-loop
     for col in cols
     for pasted-val in yanked-row
     do (let* ((col-name (vtable-column-name col))
               (col-id (cl-position col-name cols :key #'vtable-column-name :test #'string=))
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
      ;; It's tempting to use vtable-insert-object here to avoid a full refresh of the vtable.
      ;; However, we don't know what values were chosen for any columns that have a default.
      ;; This means that we can't insert at the current-row position.
      (pgmacs--display-table pgmacs--table))))


;; We can also SELECT c.column_name, c.data_type
(defun pgmacs--table-primary-keys (con table)
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
The metainformation includes the type name, whether the column is a PRIMARY KEY, whether
it is affected by constraints such as UNIQUE. Information is retrieved over the PostgreSQL
connection CON."
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
    (unless (null defaults)
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
  (lambda (fvalue max-width _table)
    (let ((truncated (if (> (string-pixel-width fvalue) max-width)
                         ;; TODO could include the ellipsis here
                         (vtable--limit-string fvalue max-width)
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
    (pg-copy-to-buffer con sql buf)))

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
      (when (y-or-n-p (format "Really run this SQL? %s" sql))
        (let ((res (pg-exec pgmacs--con sql)))
          (pgmacs--notify "%s" (pg-result res :status)))))))

(defun pgmacs--run-analyze (&rest _ignore)
  "Run ANALYZE on the current PostgreSQL table."
  (let* ((sql (format "ANALYZE %s" (pg-escape-identifier pgmacs--table)))
         (res (pg-exec pgmacs--con sql)))
    (pgmacs--notify "%s" (pg-result res :status))))


(defun pgmacs--paginated-next (&rest _ignore)
  (interactive)
  (cl-incf pgmacs--offset pgmacs-row-limit)
  (pgmacs--display-table pgmacs--table))

(defun pgmacs--paginated-prev (&rest _ignore)
  (interactive)
  (cl-decf pgmacs--offset pgmacs-row-limit)
  (pgmacs--display-table pgmacs--table))


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
Table may be specified as a string or as a schema-qualified pg-qualified-name object."
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
                     collect (make-vtable-column
                              :name (propertize name
                                                'face 'pgmacs-table-header
                                                'help-echo meta)
                              :align align
                              :min-width (1+ (max w (length name)))
                              :formatter fmt
                              :displayer (pgmacs--make-column-displayer meta))))
           (inhibit-read-only t)
           (vtable (make-vtable
                    :insert nil
                    :use-header-line nil
                    :columns columns
                    :row-colors pgmacs-row-colors
                    ;; :separator-width 1
                    ;; :separator-width (string-pixel-width " ")
                    ;; :divider-width "5px"
                    :objects rows
                    ;; same syntax for keys as keymap-set
                    :actions `("RET" (lambda (row) (pgmacs--edit-value/minibuffer row ',primary-keys))
                               "w" (lambda (row) (pgmacs--edit-value/widget row ',primary-keys))
                               "<delete>" (lambda (row) (pgmacs--delete-row row ',primary-keys))
                               "<deletechar>" (lambda (row) (pgmacs--delete-row row ',primary-keys))
                               "<backspace>" (lambda (row) (pgmacs--delete-row row ',primary-keys))
                               "DEL" (lambda (row) (pgmacs--delete-row row ',primary-keys))
                               ;; TODO: "h" to show local help
                               "+" pgmacs--insert-row
                               "i" pgmacs--insert-row/widget
                               "k" pgmacs--copy-row
                               "y" pgmacs--yank-row
                               "e" (lambda (&rest _ignored) (pgmacs-run-sql))
                               "r" pgmacs--revert-vtable
                               "j" pgmacs--row-as-json
                               ;; "n" and "p" are bound when table is paginated to next/prev page
                               "q" (lambda (&rest ignore) (kill-buffer))))))
      (setq-local pgmacs--con con
                  pgmacs--table table
                  pgmacs--offset offset
                  pgmacs--column-type-names (apply #'vector column-type-names)
                  buffer-read-only t
                  truncate-lines t)
      (when comment
        (insert (propertize "Comment" 'face 'bold))
        (insert (format ": %s" comment)))
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
        (vtable-insert vtable))
      (pgmacs--stop-progress-reporter))))


(defun pgmacs--revert-vtable (&rest _ignore)
  "Redraw the vtable in the current buffer."
  ;; We are assuming that there is a single vtable in the buffer.
  (goto-char (point-max))
  (vtable-beginning-of-table)
  (vtable-revert))

(defun pgmacs--display-backend-information (&rest _ignore)
  "Create a buffer with information concerning the current PostgreSQL backend".
  (let ((con pgmacs--con))
    (pop-to-buffer (get-buffer-create "*PostgreSQL backend information*"))
    (pgmacs-transient-mode)
    (let* ((res (pg-exec con "SELECT inet_server_addr(), inet_server_port(), pg_backend_pid()"))
           (row (pg-result res :tuple 0)))
      (insert (apply #'format "Running on %s:%s with pid %s\n" row)))
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
      (insert (apply #'format "Server timezone: %s\n" row)))))


(defvar pgmacs--stat-activity-columns
  (list "datname" "usename" "client_addr" "backend_start" "xact_start" "query_start" "wait_event"))

(defun pgmacs--display-stat-activity (&rest _ignore)
  (let* ((cols (string-join pgmacs--stat-activity-columns ","))
         (sql (format "SELECT %s FROM pg_stat_activity" cols)))
    (pgmacs-show-result pgmacs--con sql)))

;; We can't make this interactive because it's called from the keymap on a table list, where we
;; receive unnecessary arguments related to the current cursor position.
;;
;; TODO: allow input from a buffer which is set to sql-mode.
(defun pgmacs-run-sql ()
  (let ((sql (read-from-minibuffer "SQL query: ")))
    (pgmacs-show-result pgmacs--con sql)))


(defun pgmacs-show-result (con sql)
  "Create a buffer to show the results of PostgreSQL query SQL."
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
         (rows (pg-result res :tuples))
         (column-names (mapcar #'cl-first (pg-result res :attributes)))
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
                           collect (make-vtable-column
                                    :name (propertize name 'face 'pgmacs-table-header)
                                    :min-width (1+ (max w (length name)))
                                    :formatter fmt)))
         (inhibit-read-only t)
         (vtable (make-vtable
                  :insert nil
                  :use-header-line nil
                  :face 'pgmacs-table-data
                  :columns columns
                  :row-colors pgmacs-row-colors
                  ;; :separator-width 5
                  ;; :separator-width (string-pixel-width " ")
                  ;; :divider-width "5px"
                  :objects rows
                  :actions `("e" (lambda (&rest _ignored) (pgmacs-run-sql))
                             "q" (lambda (&rest _ignore) (kill-buffer))))))
    (if (null rows)
        (insert "(no rows)")
      (vtable-insert vtable))
    (pgmacs--stop-progress-reporter)))


;; Called on RET on a line in the db-list-of-tables buffer. If the cursor is on the Comment column,
;; allow the user to set the table comment. Otherwise, display the table in a separate buffer.
(defun pgmacs--dbbuf-handle-RET (table-row)
  (let* ((vtable (vtable-current-table))
         (col-id (vtable-current-column))
         (col (nth col-id (vtable-columns vtable)))
         (col-name (vtable-column-name col)))
    (cond ((string= "Comment" col-name)
           (let ((comment (read-from-minibuffer "New table comment: "))
                 (new-row (copy-sequence table-row)))
             (setf (pg-table-comment pgmacs--con (car table-row)) comment)
             (setf (nth col-id new-row) comment)
             ;; vtable-update-object doesn't work, so insert then delete old row
             (vtable-insert-object vtable new-row table-row)
             (vtable-remove-object vtable table-row)))
          ;; TODO perhaps change owner (if we are superuser)
          (t
           (pgmacs--display-table (car table-row))))))

(defun pgmacs--delete-table (table-row)
  "Delete (drop) the PostgreSQL table specified by TABLE-ROW."
  (let* ((vtable (vtable-current-table))
         (table (car table-row))
         (t-id (pg-escape-identifier table)))
    (when (yes-or-no-p (format "Really drop PostgreSQL table %s? " t-id))
      ;; We can't use a prepared statement for this dynamic SQL statement
      (let* ((sql (format "DROP TABLE %s" t-id))
             (res (pg-exec pgmacs--con sql)))
        (pgmacs--notify "%s" (pg-result res :status))
        (vtable-remove-object vtable table-row)))))

;;;###autoload
(defun pgmacs-open (con)
  "Browse the contents of PostgreSQL database to which we are connected over CON."
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
         (vtable (make-vtable
                  :insert nil
                  :use-header-line nil
                  :columns (list
                            (make-vtable-column
                             :name (propertize "Table" 'face 'pgmacs-table-header)
                             :width 20
                             :primary t
                             :align 'right)
                            (make-vtable-column
                             :name (propertize "Rows" 'face 'pgmacs-table-header)
                             :width 7 :align 'right)
                            (make-vtable-column
                             :name (propertize "Size on disk" 'face 'pgmacs-table-header)
                             :width 13 :align 'right)
                            (make-vtable-column
                             :name (propertize "Owner" 'face 'pgmacs-table-header)
                             :width 13 :align 'right)
                            (make-vtable-column
                             :name (propertize "Comment" 'face 'pgmacs-table-header)
                             :width 30 :align 'left))
                  :row-colors pgmacs-row-colors
                  :face 'pgmacs-table-data
                  ;; :column-colors '("#202020" "#404040")
                  :objects (pgmacs--list-tables)
                  :actions '("RET" pgmacs--dbbuf-handle-RET
                             ;; "h" for keybinding help
                             "<deletechar>" pgmacs--delete-table
                             "e" (lambda (&rest _ignored) (pgmacs-run-sql))
                             "q"  (lambda (&rest _ignored) (kill-buffer)))
                  :getter (lambda (object column vtable)
                            (pcase (vtable-column vtable column)
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
    (vtable-insert vtable)
    (pgmacs--stop-progress-reporter)))


;;;###autoload
(defun pgmacs-open/string (connection-string)
  "Open PGmacs on database `dbname=mydb user=me host=localhost password=foo'.
The supported keywords in the connection string are host,
hostaddr, port, dbname, user, password, sslmode (partial support)
and application_name."
  (interactive "sPostgreSQL connection string: ")
  (pgmacs--start-progress-reporter "Connecting to PostgreSQL")
  (pgmacs-open (pg-connect/string connection-string)))

;;;###autoload
(defun pgmacs-open/uri (connection-uri)
  "Open PGmacs on database `postgresql://user:pass@host/dbname'."
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


;; This is a replacement for vtable--insert-header-line, which produces poor alignment of the header
;; line.
(defun pgmacs--insert-header-line (table widths spacer)
  (cl-flet ((space-for (width)
              (propertize " " 'display (list 'space :width (list width)))))
    (let ((start (point))
          (divider (vtable-divider table))
          (cmap (define-keymap
                  "<header-line> <drag-mouse-1>" #'vtable--drag-resize-column
                  "<header-line> <down-mouse-1>" #'ignore))
          (dmap (define-keymap
                  "<header-line> <drag-mouse-1>"
                  (lambda (e)
                    (interactive "e")
                    (vtable--drag-resize-column e t))
                  "<header-line> <down-mouse-1>" #'ignore)))
      (seq-do-indexed
       (lambda (column index)
         (let* ((name (propertize
                       (vtable-column-name column)
                       'face (list 'pgmacs-table-header)
                       'mouse-face 'header-line-highlight
                       'keymap cmap))
                (start (point))
                (indicator (vtable--indicator table index))
                (indicator-width (string-pixel-width indicator))
                (last (= index (1- (length (vtable-columns table)))))
                (displayed (if (> (string-pixel-width name)
                                  (- (elt widths index) indicator-width))
                               (vtable--limit-string
                                name (- (elt widths index) indicator-width))
                             name)))
           (let ((fill-width (- (elt widths index)
                                (string-pixel-width displayed)
                                indicator-width)))
             (if (eq (vtable-column-align column) 'left)
                 (insert displayed (space-for fill-width) indicator)
               (insert (space-for fill-width) displayed indicator)))
           (unless last
             (insert (space-for spacer)))
           (when (and divider (not last))
             (insert (propertize divider 'keymap dmap)))
           (put-text-property start (point) 'vtable-column index)))
       (vtable-columns table))
      (insert "\n")
      (add-face-text-property start (point) 'header-line))))

(defun pgmacs--insert-header-line-replace (_orig-fun &rest args)
  (apply #'pgmacs--insert-header-line args))

(advice-add 'vtable--insert-header-line :around #'pgmacs--insert-header-line-replace)


(provide 'pgmacs)

;;; pgmacs.el ends here
