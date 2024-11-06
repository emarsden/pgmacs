;;; pgmacs.el --- Emacs is editing a PostgreSQL database  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Eric Marsden
;; Author: Eric Marsden <eric.marsden@risk-engineering.org>
;; Version: 0.16
;; Package-Requires: ((emacs "29.1") (pg "0.39"))
;; URL: https://github.com/emarsden/pgmacs/
;; Keywords: data, PostgreSQL, database
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; See README.md at https://github.com/emarsden/pgmacs/


;;; Code:

(require 'cl-lib)
(require 'button)
(require 'widget)
(require 'wid-edit)
(require 'cus-edit)
(require 'svg)
(require 'rx)
(require 'sql)
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

(defface pgmacs-column-foreign-key
  '((t (:inherit pgmacs-table-data
        :foreground "blue")))
  "Face used to display data in a column that references another table."
  :group 'pgmacs)

(defface pgmacs-column-primary-key
  '((t (:inherit pgmacs-table-data
                 :bold t)))
  "Face used to display data in a column that is part of a primary key."
  :group 'pgmacs)

(defface pgmacs-table-header
  '((((class color) (background light))
     :bold t
     :background "lightgray"
     :foreground "darkolivegreen")
    (((class color) (background dark))
     :bold t
     :background "darkolivegreen"
     :foreground "lightgreen"))
  "Face used to display a PGmacs database table header."
  :group 'pgmacs)

(defface pgmacs-where-filter
  '((((class color) (background light))
     ;; This is a pale purple color
     :background "#E6E6FA")
    (((class color) (background dark))
     :foreground "#E6E6FA"))
  "Face used to display a PGmacs WHERE filter in a row-list buffer."
  :group 'pgmacs)

(defcustom pgmacs-row-colors
  '("#D9CEB4" "#D9B96C")
  "The colors used for alternating rows in a database table."
  :type '(list color color)
  :group 'pgmacs)

(defcustom pgmacs-row-limit 200
  "The maximum number of rows to retrieve per database query.
If more rows are present in the PostgreSQL query result, the display of results
will be paginated.  You may wish to set this to a low value if accessing
PostgreSQL over a slow network link."
  :type 'number
  :group 'pgmacs)

(defcustom pgmacs-max-column-width 60
  "The maximal width in characters of a table column."
  :type 'number
  :group 'pgmacs)

(defcustom pgmacs-enable-query-logging nil
  "Whether SQL queries sent to PostgreSQL should be logged."
  :type 'boolean
  :group 'pgmacs)

;; To run SchemaSpy on the current database or on the current table, and display images describing
;; the schema. The default setting runs SchemaSpy in a Docker/Podman software container, using
;; Podman or Docker. This is the easiest way of running SchemaSpy, because all necessary
;; dependencies are preinstalled. Alternatively (see commented commandline), you can run the
;; SchemaSpy java application natively, which requires the following software to be installed:
;;
;;  - SchemaSpy (see schemaspy.org)
;;  - Java
;;  - GraphViz
;;  - JDBC support for PostgreSQL (here in /usr/share/java/postgresql-jdbc4.jar, installable for example
;;    using "sudo apt install libpostgresql-jdbc-java")
;;
;; The commandline below with "--userns=keep-id" is suitable for running a container in rootless
;; mode with Podman. This is because the SchemaSpy Dockerfile creates a new user "java" then runs as
;; that user. We can't map that new user id to our own id, because the java uid is not known prior
;; to the container running. With the --userns=keep-id option however, the USER will be mapped to
;; our own uid when running rootless.
(defcustom pgmacs-schemaspy-cmdline
  "podman run -v %D:/output --userns=keep-id --network=host docker.io/schemaspy/schemaspy:latest -t pgsql11 -host %h -port %P -u %u -p %p -db %d -imageformat svg"
  "Commandline for running the SchemaSpy application or container.

This commandline will be used both for generating an image
representing the relations between tables in the current
database, and for generating an image representing the schema of
the current table. In the latter case, the string ` -s %s -i %t'
will be appended to the commandline (with `%s' replaced by the
table schema name and `%t' by the table name).

SchemaSpy can be run as a Java application installed on the local
machine, or (probably easier for most users) in a Docker/Podman
software container that contains the necessary dependencies.

In this commandline, %d is replaced by the database name, %h by
the hostname on which PostgreSQL is running, %P by the port it is
running on, %u by the user, %p by the password, %s by the current
table schema name, %t by the current table name and %D by the
directory (which will be created in the system temporary
directory) in which output files are created by SchemaSpy. The %s
and %t values will only be used when generating illustrations
concerning a specific table, rather than the entire database."
  :type 'string
  :group 'pgmacs)


(defun pgmacs--maybe-svg-icon (svg-fn)
  (if (and (display-graphic-p)
           (image-type-available-p 'svg))
      (let ((svg (funcall svg-fn)))
	(propertize " " 'display svg 'rear-nonsticky t 'cursor-intangible t))
    ""))

(defcustom pgmacs-use-header-line t
  "If non-nil, use header line to display information on PostgreSQL connection."
  :type 'boolean
  :group 'pgmacs)

(defvar pgmacs-header-line
  (list (when (char-displayable-p ?🐘) " 🐘")
        (propertize " PGmacs " 'face 'bold)
        '(:eval (when pgmacs--con
                 ;; (list :tcp host port dbname user password)
                 (let ((ci (pgcon-connect-info pgmacs--con))
                       (tls (cl-first
                             (pg-result
                              (pg-exec pgmacs--con "SHOW ssl") :tuple 0)))
                       (maybe-icon (pgmacs--maybe-svg-icon #'pgmacs--svg-icon-user)))
                   (cl-case (cl-first ci)
                     (:tcp
                      (format "%s as %s%s on %s:%s (TLS: %s)"
                              (propertize (cl-fourth ci) 'face 'bold)
                              maybe-icon
                              (cl-fifth ci)
                              (cl-second ci)
                              (cl-third ci)
                              tls))
                     (:local
                      (format "%s as %s%s on Unix socket"
                              (propertize (cl-fourth ci) 'face 'bold)
                              maybe-icon
                              (cl-fifth ci))))))))
  "Header-line to use in PGmacs buffers. Nil to disable.")

(defcustom pgmacs-mode-hook nil
  "Mode hook for `pgmacs-mode'."
  :type 'hook
  :group 'pgmacs)

(defun pgmacs--widget-setup ()
  "Set up the appearance of widgets used in PGmacs.
Uses customizations implemented in Emacs' customize support."
  ;; cus-edit.el has this rather rude keymap binding which adds to widget-field-keymap
  ;; bindings that are not relevant to us. So we revert it back to widget-field-keymap.
  (widget-put (get 'editable-field 'widget-type) :keymap widget-field-keymap)
  (setq-local widget-button-face custom-button)
  (setq-local widget-button-pressed-face custom-button-pressed)
  (setq-local widget-mouse-face custom-button-mouse)
  (when custom-raised-buttons
    (setq-local widget-push-button-prefix "")
    (setq-local widget-push-button-suffix "")
    (setq-local widget-link-prefix "")
    (setq-local widget-link-suffix "")))

(defvar-keymap pgmacs-table-list-map
  :doc "Keymap for PGmacs table-list buffers"
  (kbd "q") #'bury-buffer
  (kbd "h") #'pgmacs--table-list-help
  (kbd "?") #'pgmacs--table-list-help
  (kbd "g") #'pgmacs--table-list-redraw
  (kbd "o") #'pgmacs-open-table
  (kbd "e") #'pgmacs-run-sql
  (kbd "E") #'pgmacs-run-buffer-sql
  (kbd "T") #'pgmacs--switch-to-database-buffer)

(define-key global-map [menu-bar tools PGmacs]
   (cons "PGmacs" (make-sparse-keymap "PGmacs")))

(define-key global-map [menu-bar tools PGmacs open-uri]
   '("Open PostgreSQL URI" . pgmacs-open-uri))

(define-key global-map [menu-bar tools PGmacs open-string]
   '("Open PostgreSQL connection string" . pgmacs-open-string))

(define-key pgmacs-table-list-map [menu-bar tools PGmacs run-sql]
   '("Run SQL query" . pgmacs-run-sql))


(defun pgmacs-mode ()
  "Mode for browsing and editing data in a PostgreSQL database.
PGmacs provides an editing interface for PostgreSQL. The main
PGmacs table-list buffer that is displayed when you connect to a
database backend allows you to:
 - browse the list of tables in the database
 - browse/edit a table (type `RET' on the table name or `o' to be
   prompted for a table name in the minibuffer)
 - delete a table (type `DEL' on the table name)
 - rename a table (type `r' on the table name)
 - modify the SQL comment on a table (type `RET' in the `comment' column)
 - show the output from an SQL query in table mode (type `e' to enter the
   SQL query in the minibuffer)
 - run SchemaSpy on the database to view its structure (type `S', only
   available in graphical mode)
 - type `h' to show buffer-specific help and keybindings

In a row-list buffer, which displays the rows of data in a
database table along with metainformation on the table (column
types and associated SQL constraints, on-disk size, table owner),
you can:
 - browse the table contents row by row, in paginated mode for large
   tables. Type `n' and `p' to move to the next/previous page in a
   paginated buffer.
 - use `M-right' and `M-left' to move to the next/previous column,
   type a number to move to that column (numbering is zero-based)
 - edit the data value at point (type `RET' on the value you want to
   modify to edit it in the minibuffer, or `w' to open a dedicated
   widget-based editing buffer)
 - insert a new row by typing `+' (you will be prompted in the minibuffer
   for new values, unless an SQL default value is defined) or by typing
   `i' (opens a dedicated widget-based buffer for you to enter the new
   values).
 - copy the current row to the kill ring in JSON format (type `j' on the
   row you want to serialize to JSON)
 - delete a row (type `DEL' on the row you wish to delete)
 - copy/paste rows of a database table (type `k' to copy, `y' to paste)
 - export the contents of a table to CSV using a dedicated button
 - type `S' to run SchemaSpy on the current table and display its structure
 - type `o' to open a new row-list buffer for another table
 - type `T' to jump back to the main table-list buffer
 - type `h' to show buffer-specific help and keybindings

Please note that edits, insertions and deletions are made immediately on
the live PostgreSQL database.

See the `pgmacs' customization group for a list of user options.

Entering this mode runs the functions on `pgmacs-mode-hook'.
"
  (setq major-mode 'pgmacs-mode
        mode-name "PGmacs")
  (when (and pgmacs-use-header-line pgmacs-header-line)
    (setq header-line-format pgmacs-header-line))
  ;; Not appropriate for user to type stuff into our buffers.
  (put 'pgmacs-mode 'mode-class 'special)
  (use-local-map pgmacs-table-list-map)
  (pgmacs--widget-setup)
  (cursor-intangible-mode 1)
  (buffer-disable-undo)
  (run-mode-hooks 'pgmacs-mode-hook))

(defvar-keymap pgmacs-row-list-map
  :doc "Keymap for PGmacs row-list buffers"
  (kbd "q") #'bury-buffer
  (kbd "h") #'pgmacs--row-list-help
  (kbd "?") #'pgmacs--row-list-help
  (kbd "g") #'pgmacs--row-list-redraw
  (kbd "i") #'pgmacs--insert-row-empty
  (kbd "o") #'pgmacs-open-table
  (kbd "e") #'pgmacs-run-sql
  (kbd "E") #'pgmacs-run-buffer-sql
  (kbd "W") #'pgmacs--add-where-filter
  (kbd "S") #'pgmacs--schemaspy-table
  (kbd "T") #'pgmacs--switch-to-database-buffer)

(defvar-keymap pgmacs-transient-map
  :doc "Keymap for PGmacs transient buffers"
  (kbd "q") #'bury-buffer
  (kbd "o") #'pgmacs-open-table
  (kbd "e") #'pgmacs-run-sql
  (kbd "E") #'pgmacs-run-buffer-sql
  (kbd "T") #'pgmacs--switch-to-database-buffer)

(define-minor-mode pgmacs-transient-mode
  "Minor mode for transient PGmacs buffers."
  :global nil
  :init-value nil
  :keymap pgmacs-transient-map)

(defvar-keymap pgmacs-paginated-map
  :doc "Additional keymap for paginated PGmacs row-list buffers"
  (kbd "n") #'pgmacs--paginated-next
  (kbd "p") #'pgmacs--paginated-prev)

(define-minor-mode pgmacs-paginated-mode
  "Minor mode for paginated PGmacs table buffers."
  :global nil
  :init-value nil
  :keymap pgmacs-paginated-map)


;; Used for updating on progress retrieving information from PostgreSQL.
;; FIXME: these should be per-PostgreSQL-connection rather than per-Emacs-instance.
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

;; TODO: it would be cleaner to hold these all in a pgmacs-connection object.
(defvar-local pgmacs--con nil)
(defvar-local pgmacs--table nil)
(defvar-local pgmacs--column-type-names nil)
(defvar-local pgmacs--offset nil)
(defvar-local pgmacs--db-buffer nil)
(defvar-local pgmacs--where-filter nil)
(defvar-local pgmacs--marked-rows (list))
(defvar-local pgmacs--completions nil)

(defvar pgmacs--column-display-functions (make-hash-table :test #'equal))

;; Allow the user to register a dedicated display function for a particular column in a particular
;; table. The display-function takes three arguments: the cell-value, max-width, table.
;;
;; This functionality can be used to display BYTEA columns as inline images, for example.
(defun pgmacs-register-column-displayer (table column display-function)
  (puthash (cons table column) display-function pgmacs--column-display-functions))

(defun pgmacs--lookup-column-displayer (table column)
  (gethash (cons table column) pgmacs--column-display-functions nil))


;; We can have several table-list buffers open, corresponding to different PostgreSQL databases. The
;; buffer-local pgmacs--db-buffer is kept up to date in each PGmacs buffer to point to its main
;; table-list buffer.
(defun pgmacs--switch-to-database-buffer (&rest _ignore)
  "Switch to the main table-list buffer for the current PostgreSQL database."
  (interactive)
  (switch-to-buffer pgmacs--db-buffer))

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
         #'char-to-string)
        ((string= type-name "hstore")
         (lambda (ht)
           (let ((items (list)))
             (maphash (lambda (k v) (push (format "\"%s\"=>\"%s\"" k v) items)) ht)
             (string-join items ","))))
        ((string= type-name "json")
         #'json-serialize)
        ((string= type-name "jsonb")
         #'json-serialize)
        ((string= type-name "vector")
         (lambda (val)
           (concat "[" (string-join (mapcar #'prin1-to-string val) ",") "]")))
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
        ((string= type-name "char2") 4)
        ((string= type-name "char4") 6)
        ((string= type-name "char8") 10)
        ((string= type-name "char16") 20)
        ((string= type-name "text") 25)
        ((string= type-name "varchar") 25)
        ((string= type-name "bpchar") 7)
        ((string= type-name "name") 25)
        ((string= type-name "bytea") 10)
        ((string= type-name "json") 40)
        ((string= type-name "jsonb") 40)
        ((string= type-name "uuid") 36)
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
             (if (and schema (not (string= "public" schema)))
                 (format "%s.%s"
                         (user-facing schema)
                         (user-facing name))
               (user-facing name))))
          (t
           (user-facing name)))))

(defun pgmacs--row-as-json (current-row)
  "Copy the CURRENT-ROW as JSON to the kill ring."
  (unless (json-available-p)
    (message "Emacs is not compiled with JSON support")
    (cl-return-from pgmacs--row-as-json))
  (cl-labels ((jsonable-p (val)
                (cl-typecase val
                  (number t)
                  (string t)
                  (vector (cl-every #'jsonable-p val))
                  (hash-table t)
                  (t nil))))
    (let* ((tbl (pgmacstbl-current-table))
           (cols (pgmacstbl-columns tbl))
           (ht (make-hash-table :test #'equal))
           (ce (pgcon-client-encoding pgmacs--con)))
      (cl-loop
       for col-id from 0
       for col in cols
       for col-type = (aref pgmacs--column-type-names col-id)
       for raw in current-row
       for v = (if (jsonable-p raw) raw
                 (pg-serialize raw col-type ce))
       do (puthash (pgmacstbl-column-name col) v ht))
      (kill-new (json-serialize ht))
      (message "JSON copied to kill ring"))))

(defun pgmacs--read-value-minibuffer (name type prompt current-value)
  "Read a value for column NAME in the minibuffer using PROMPT.
The column has SQL type TYPE and has current value CURRENT-VALUE."
  (let* ((prompt (format prompt name type)))
    (read-string prompt (and current-value (format "%s" current-value)))))

;; TODO: perhaps if the field value is very long or of BYTEA type, prompt "really want to edit in
;; minibuffer" and suggest using the widget editing mode instead.
(defun pgmacs--read-value (name type prompt current-value)
  "Read a value for column NAME of SQL type TYPE.
Use PROMPT in the minibuffer and show the current value CURRENT-VALUE."
  (let* ((user-provided (pgmacs--read-value-minibuffer name type prompt current-value))
         (parser (pg-lookup-parser type))
         (ce (pgcon-client-encoding pgmacs--con)))
    (if parser (funcall parser user-provided ce) user-provided)))

(defun pgmacs-funcall-cell (function)
  "Call FUNCTION on the content of current cell. Does not modify database.
FUNCTION takes a single argument which is the value of the cell at point."
  (let* ((col-id (or (pgmacstbl-current-column)
                     (user-error "Not on a pgmacstbl column")))
         (current-row (or (pgmacstbl-current-object)
                          (user-error "Cursor is not on a pgmacstbl row")))
         (current (nth col-id current-row)))
    (funcall function current)))

(defun pgmacs--setf-cell (row primary-keys function)
  "Call FUNCTION on current cell and update the database.
FUNCTION is called on (old-value col-name col-type) and returns the new value."
  (let* ((pgmacstbl (or (pgmacstbl-current-table)
                        (user-error "Cursor is not in a pgmacstbl")))
         (current-row (or (pgmacstbl-current-object)
                          (user-error "Cursor is not on a pgmacstbl row")))
         (cols (pgmacstbl-columns pgmacstbl))
         (col-id (or (pgmacstbl-current-column)
                     (user-error "Not on a pgmacstbl column")))
         (col (nth col-id cols))
         (col-name (pgmacstbl-column-name col))
         (col-type (aref pgmacs--column-type-names col-id))
         (pk (cl-first primary-keys))
         (pk-col-id (or (cl-position pk cols :key #'pgmacstbl-column-name :test #'string=)
                        (error "Can't find primary key %s in the pgmacstbl column list" pk)))
         (pk-col-type (and pk-col-id (aref pgmacs--column-type-names pk-col-id)))
         (pk-value (and pk-col-id (nth pk-col-id row))))
    (let* ((current (funcall (pgmacstbl-column-formatter col)
                             (nth col-id current-row)))
           (new-value (funcall function current col-name col-type)))
      (when (null primary-keys)
        (user-error "Can't edit content of a table that has no PRIMARY KEY"))
      (unless pk-value
        (error "Can't find value for primary key %s" pk))
      (let* ((sql (format "UPDATE %s SET %s = $1 WHERE %s = $2"
                          (pg-escape-identifier pgmacs--table)
                          (pg-escape-identifier col-name)
                          (pg-escape-identifier pk)))
             (res (pg-exec-prepared pgmacs--con sql
                                    `((,new-value . ,col-type)
                                      (,pk-value . ,pk-col-type)))))
        (pgmacs--notify "%s" (pg-result res :status)))
      (let ((new-row (copy-sequence current-row)))
        (setf (nth col-id new-row) new-value)
        ;; pgmacstbl-update-object doesn't work, so insert then delete old row
        (pgmacstbl-insert-object pgmacstbl new-row current-row)
        (pgmacstbl-remove-object pgmacstbl current-row)
        ;; redrawing is necessary to ensure that all keybindings are present for the newly inserted
        ;; row.
        (forward-line -1)
        (pgmacs--redraw-pgmacstbl))
      new-value)))

(defun pgmacs--edit-value-minibuffer (row primary-keys)
  "Edit and update in PostgreSQL the column value at point.
The new value in database row ROW is read in the minibuffer.
Editing requires the database table to have primary keys named in the list
PRIMARY-KEYS."
  (unless primary-keys
    (user-error "Cannot edit a table that has no PRIMARY KEY"))
  (let ((get-value (lambda (old-value col-name col-type)
                     (pgmacs--read-value (substring-no-properties col-name)
                                         (substring-no-properties col-type)
                                         "Change %s (%s) to: "
                                         old-value))))
  (pgmacs--setf-cell row primary-keys get-value)))


(defvar pgmacs--shell-command-history nil)

(defun pgmacs--shell-command-on-value (current-row primary-keys)
  "Run a Unix filter shell command with the current cell value as input.

When called without a prefix argument, output is diplayed in the echo area.
When called with a prefix argument, replace the current cell value with the output
(updating the database).

For example, to count the number of characters in the current cell,

   ! wc -c

To downcase the value of a text cell (and modify the value in the database) use

   C-u ! tr '[:upper:]' '[:lower]'

To reverse the order of the characters in the cell (and modify the value in
the database), use

   C-u ! rev

Works on the CURRENT-ROW and on a table with PRIMARY-KEYS."
  (let ((get-value
         (lambda (old-value &rest _ignore)
           (let* ((prompt "Shell command: ")
                  (cmd (read-string prompt nil 'pgmacs--shell-command-history))
                  (new-value (with-temp-buffer
                               (insert old-value)
                               (let ((status (shell-command-on-region
                                              (point-min) (point-max) cmd t t
                                              " *PGmacs shell command error*" t)))
                                 (when (zerop status)
                                   (buffer-substring-no-properties (point-min) (point-max)))))))
             (unless new-value
               (error "Shell command failed: check *PGmacs shell command error* buffer"))
             new-value))))
    (if (not current-prefix-arg)
        (let ((result (pgmacs-funcall-cell get-value)))
          (message "PostgreSQL shell: %s" result))
      ;; With a prefix argument, we update the value in the PostgreSQL database and in the
      ;; displayed pgmacstbl.
      (pgmacs--setf-cell current-row primary-keys get-value))))

(defvar pgmacs--async-command-history nil)

(defun pgmacs--async-command-on-value (&rest _ignore)
  "Run a command asynchronously with the current cell value as first argument.
The command should be the name of a program, which will be searched for in
`exec-path' (it is run via `start-process', without a shell). Command output will
be displayed in a buffer called *PGmacs async command*.

For example, if the cell contains a filename, you can open the filename in the
default application on your system by entering `xdg-open' (or `open' on a MacOS
machine).

Works on the CURRENT-ROW and on a table with PRIMARY-KEYS."
  (pgmacs-funcall-cell
   (lambda (cell-value)
     (let* ((prompt "Async command: ")
            (cmd (read-string prompt nil 'pgmacs--async-command-history))
            (buf (get-buffer-create "*PGmacs async command*")))
       (start-process "PGmacs-async-command" buf cmd cell-value)))))

(defun pgmacs--downcase-value (current-row primary-keys)
  "Downcase the value in the cell at point and update PostgreSQL.
Operates on the CURRENT-ROW and on a table with PRIMARY-KEYS."
  (let ((get-value
         (lambda (old-value _col-name col-type)
           (unless (or (string= "text" col-type)
                       (string= "varchar" col-type)
                       (string= "name" col-type))
             (user-error "Can only downcase text values"))
           (with-temp-buffer
             (insert old-value)
             (downcase-region (point-min) (point-max))
             (buffer-substring-no-properties (point-min) (point-max))))))
    (pgmacs--setf-cell current-row primary-keys get-value)))

(defun pgmacs--upcase-value (current-row primary-keys)
  "Upcase the value in the cell at point and update PostgreSQL.
Operates on the CURRENT-ROW and on a table with PRIMARY-KEYS."
  (let ((get-value
         (lambda (old-value _col-name col-type)
           (unless (or (string= "text" col-type)
                       (string= "varchar" col-type)
                       (string= "name" col-type))
             (user-error "Can only upcase text values"))
           (with-temp-buffer
            (insert old-value)
            (upcase-region (point-min) (point-max))
            (buffer-substring-no-properties (point-min) (point-max))))))
    (pgmacs--setf-cell current-row primary-keys get-value)))

(defun pgmacs--capitalize-value (current-row primary-keys)
  "Capitalize the value in the cell at point and update PostgreSQL.
Operates on the CURRENT-ROW and on a table with PRIMARY-KEYS."
  (let ((get-value
         (lambda (old-value _col-name col-type)
           (unless (or (string= "text" col-type)
                       (string= "varchar" col-type)
                       (string= "name" col-type))
             (user-error "Can only capitalize text values"))
           (with-temp-buffer
             (insert old-value)
             (capitalize-region (point-min) (point-max))
             (buffer-substring-no-properties (point-min) (point-max))))))
    (pgmacs--setf-cell current-row primary-keys get-value)))


(define-widget 'pgmacs-hstore-widget 'list
  "Widget to edit a PostgreSQL HSTORE key-value map."
  :tag "HSTORE key-value mapping"
  :format "%v"
  :offset 2
  ;; Convert from the hashtable format used to represent an hstore column in Emacs Lisp to an alist
  ;; that is suitable for display using our widget.
  :value-to-internal (lambda (_widget ht)
                       (let* ((entries (list)))
                         (maphash (lambda (k v) (push (cons k v) entries)) ht)
                         entries))
  ;; Convert from the alist display format back to a hashtable.
  :value-to-external (lambda (_widget alist)
                       (let ((ht (make-hash-table :test #'equal)))
                         (dolist (kv alist)
                           (puthash (car kv) (cdr kv) ht))
                         ht))
  :args '((editable-list :inline t
                         ;; don't display the [INS] button on each line (which would be inserted if
                         ;; the %i escape were present here), because the order of entries in the
                         ;; HSTORE is not relevant.
                         :entry-format "%d %v\n"
                         (cons :format "%v"
                               (editable-field :size 25 :tag "Key" :format "%v ⟶ ")
                               (editable-field :size 40 :tag "Value")))))

(define-widget 'pgmacs-json-widget 'text
  "Widget to edit PostgreSQL JSON/JSONB values."
  :tag "JSON/JSONB value"
  :format "%v"
  :offset 2
  ;; The pg-el library deserializes JSON and JSONB values to hashtables using Emacs' native JSON
  ;; support. Here we use the native JSON support plus pretty-printing support from json.el.
  :value-to-internal (lambda (_widget ht)
                       (with-temp-buffer
                         (insert (json-serialize ht))
                         (json-pretty-print-buffer)
                         (buffer-string)))
  :value-to-external (lambda (_widget str) (json-parse-string str))
  :args '((string :inline t :size 500)))

;; function org-read-date provides a convenient interface
;; and see complicated widget lib https://github.com/aki2o/emacs-date-field/blob/master/date-field.el
(define-widget 'pgmacs-date-widget 'text
  "Widget to edit a PostgreSQL date column."
  :tag "Date"
  :format "%v"
  :offset 2
  :valid-regexp (rx (+ digit) ?- (repeat 2 digit) ?- (repeat 2 digit))
  :value-to-internal (lambda (_widget val)
                       (format-time-string "%Y-%m-%d" val))
  :value-to-external (lambda (_widget str)
                       (pg-date-parser str nil))
  :args '((string :inline t :size 20)))

;; https://www.postgresql.org/docs/current/datatype-uuid.html
(define-widget 'pgmacs-uuid-widget 'list
  "Widget to edit a PostgreSQL UUID column."
  :tag "UUID"
  :format "%v"
  :offset 2
  :value-to-internal (lambda (_widget uuid-string)
                       (split-string uuid-string "-"))
  :value-to-external (lambda (_widget uuid-elements)
                       (apply #'format (cons "%s-%s-%s-%s-%s" uuid-elements)))
  :error "Invalid format for a UUID."
  :args '((list :inline t :format "%v"
                (editable-field :size 8
                                :valid-regexp (rx (repeat 8 xdigit))
                                :error "Invalid format for UUID component"
                                :format "%v-")
                (editable-field :size 4 :valid-regexp (rx (repeat 4 xdigit)) :format "%v-")
                (editable-field :size 4 :valid-regexp (rx (repeat 4 xdigit)) :format "%v-")
                (editable-field :size 4 :valid-regexp (rx (repeat 4 xdigit)) :format "%v-")
                (editable-field :size 12 :valid-regexp (rx (repeat 12 xdigit)) :format "%v"))))

(defun pgmacs--widget-for (type current-value)
  "Create a widget for TYPE and CURRENT-VALUE in the current buffer."
  (cond ((string= "bool" type)
         (widget-create 'boolean (or current-value nil)))
        ((or (string= "smallint" type)
             (string= "int2" type)
             (string= "int4" type)
             (string= "int8" type)
             (string= "oid" type))
         (widget-create 'integer (or current-value "")))
        ((or (string= type "numeric")
             (string= type "float4")
             (string= type "float8"))
         (widget-create 'float (or current-value "")))
        ((string= type "char")
         (widget-create 'character (or current-value "")))
        ;; blank-trimmed text
        ((string= type "bpchar")
         (widget-create 'character (or current-value "")))
        ((or (string= "text" type)
             (string= "varchar" type))
         (let ((default (or current-value "")))
           (widget-create 'string
                          :size (max 80 (min 200 (+ 5 (length default))))
                          :value default)))
        ;; represented as "[44,33,5,78]" on the wire. Parsed to an elisp vector of integers.
        ((string= "vector" type)
         (widget-create '(vector integer) :value (or current-value (vector))))
        ((string= "hstore" type)
         (widget-create 'pgmacs-hstore-widget :value (or current-value (make-hash-table))))
        ((or (string= "json" type)
             (string= "jsonb" type))
         (widget-create 'pgmacs-json-widget :value (or  current-value (make-hash-table))))
        ((string= "date" type)
         (widget-create 'pgmacs-date-widget (or current-value "")))
        ;; TODO: timestamp, timestamptz
        ((string= "uuid" type)
         (widget-create 'pgmacs-uuid-widget :value (or current-value "")
                        :action (lambda (wid &rest _ignore)
                                  (if (widget-apply wid :validate)
                                      (user-error "Invalid UUID: %s" (widget-get wid :error))
                                    (message "%s is ok" (widget-value wid))))))
        (t
         (widget-create 'editable-field
                        :size (min 200 (+ 5 (length current-value)))
                        (format "%s" current-value)))))

(defun pgmacs--edit-value-widget (row primary-keys)
  "Edit and update in PostgreSQL the value at point in ROW.
Uses a dedicated widget buffer.  Editing is only possible if the current table
has primary keys, named in the list PRIMARY-KEYS."
  (when (null primary-keys)
    (user-error "Can't edit content of a table that has no PRIMARY KEY"))
  (let* ((con pgmacs--con)
         (db-buffer pgmacs--db-buffer)
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
    (let* ((current (nth col-id current-row))
           (sql (format "UPDATE %s SET %s = $1 WHERE %s = $2"
                        (pg-escape-identifier pgmacs--table)
                        (pg-escape-identifier col-name)
                        (pg-escape-identifier pk)))
           (updater (lambda (user-provided)
                      (let* ((parser (pg-lookup-parser col-type))
                             (ce (pgcon-client-encoding con))
                             ;; Some of the input widgets we use return a pre-parsed type (e.g. a
                             ;; floating point number) rather than a string
                             (new-value (if (and (stringp user-provided) parser)
                                            (funcall parser user-provided ce)
                                          user-provided))
                             (res (pg-exec-prepared con sql
                                                    `((,new-value . ,col-type)
                                                      (,pk-value . ,pk-col-type)))))
                        (pgmacs--notify "%s" (pg-result res :status))
                        (let ((new-row (copy-sequence current-row)))
                          (setf (nth col-id new-row) new-value)
                          ;; pgmacstbl-update-object doesn't work, so insert then delete old row
                          (pgmacstbl-insert-object pgmacstbl new-row current-row)
                          (pgmacstbl-remove-object pgmacstbl current-row)
                          (forward-line -1)
                          (pgmacs--redraw-pgmacstbl))))))
      (switch-to-buffer "*PGmacs update widget*")
      (erase-buffer)
      (remove-overlays)
      (kill-all-local-variables)
      (setq-local pgmacs--con con
                  pgmacs--db-buffer db-buffer
                  pgmacs--table table)
      (when pgmacs-use-header-line
	(setq-local header-line-format (format "%sUpdate PostgreSQL column %s"
                                               (if (char-displayable-p ?🐘) " 🐘" "")
                                               col-name)))
      (pgmacs-mode)
      (widget-insert "\n")
      (let* ((column-info (pgmacs--column-info con table col-name))
             (formatted-info (pgmacs--format-column-info column-info)))
        (widget-insert (format "  Column type: %s\n\n" formatted-info)))
      (widget-insert (format "  Change %s for current row to:" (substring-no-properties col-name)))
      (widget-insert "\n\n")
      (let* ((w-updated (pgmacs--widget-for col-type current))
             (update-action (lambda (&rest _ignore)
                                (interactive)
                                ;; FIXME this :validate is always returning t
;;                                 (when (widget-apply w-updated :validate)
;;                                   (message "Widget %s failed verification: %s"
;;                                            (widget-get w-updated :tag)
;;                                            (widget-get w-updated :error)))
                                (let ((updated (widget-value w-updated)))
                                  (kill-buffer (current-buffer))
                                  (funcall updater updated)))))
        (widget-insert "\n\n")
        (widget-create 'push-button
                       :offset 2
                       :notify update-action
                       "Update database")
        (widget-insert "\n\n\n")
        (widget-insert (propertize "(To abort editing the column value, simply kill this buffer.)"
                                   'face 'font-lock-comment-face))
        (widget-insert "\n")
        (widget-setup)
        (let ((map (make-sparse-keymap)))
          (set-keymap-parent map widget-keymap)
          (define-key map (kbd "C-c C-c") update-action)
          (use-local-map map))
        ;; map over all widget children/fields and add binding for C-c C-c to their existing keymap
        ;; (widget-put w-updated :keymap map))
        (goto-char (point-min))
        (widget-forward 1)))))

(defun pgmacs--view-value (current-row)
  "Insert column value at point into a dedicated buffer.
Point is located in CURRENT-ROW."
  (let* ((db-buffer pgmacs--db-buffer)
         (col-id (pgmacstbl-current-column))
         (cols (pgmacstbl-columns (pgmacstbl-current-table)))
         (col (nth col-id cols))
         (col-name (pgmacstbl-column-name col))
         (buf (get-buffer-create (format "*PostgreSQL column value %s*" col-name)))
         (value (funcall (pgmacstbl-column-formatter col)
                         (nth col-id current-row))))
    (erase-buffer)
    (pop-to-buffer buf)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (remove-overlays)
      (kill-all-local-variables)
      (insert value))
    (setq-local pgmacs--db-buffer db-buffer)
    (pgmacs-transient-mode)
    (shrink-window-if-larger-than-buffer)
    (setq buffer-read-only t)
    (goto-char (point-min))))

(defun pgmacs--delete-row (row primary-keys)
  "Delete ROW from the current table.
Deletion is only possible when the current table has primary
keys, whose names are given by the list PRIMARY-KEYS."
  (when (null primary-keys)
    (user-error "Can't edit content of a table that has no PRIMARY KEY"))
  (when (y-or-n-p (format "Really delete PostgreSQL row %s?" row))
    (let* ((pgmacstbl (pgmacstbl-current-table))
           (cols (pgmacstbl-columns pgmacstbl))
           (where-clauses (list))
           (where-values (list))
           (counter 0))
      (dolist (pk primary-keys)
        (let* ((col-name (cl-position pk cols :key #'pgmacstbl-column-name :test #'string=))
               (col-type (and col-name (aref pgmacs--column-type-names col-name)))
               (value (and col-name (nth col-name row))))
          (unless value
            (error "Can't find value for primary key %s" pk))
          (push (format "%s = $%d" (pg-escape-identifier pk) (cl-incf counter)) where-clauses)
          (push (cons value col-type) where-values)))
      (setq where-clauses (nreverse where-clauses)
            where-values (nreverse where-values))
      (let* ((sql (format "DELETE FROM %s WHERE %s"
                          (pg-escape-identifier pgmacs--table)
                          (string-join where-clauses " AND ")))
             (_ (pg-exec pgmacs--con "START TRANSACTION"))
             (res (pg-exec-prepared pgmacs--con sql where-values))
             (status (pg-result res :status)))
        (pgmacs--notify "%s" status)
        (unless (string= "DELETE " (substring status 0 7))
          (error "Unexpected status %s for PostgreSQL DELETE command" status))
        (let ((rows (cl-parse-integer (substring status 7))))
          (cond ((eql 0 rows)
                 (warn "Could not delete PostgreSQL row")
                 (pg-exec pgmacs--con "COMMIT TRANSACTION"))
                ((eql 1 rows)
                 (pg-exec pgmacs--con "COMMIT TRANSACTION")
                 (pgmacstbl-remove-object pgmacstbl row)
                 (pgmacs--redraw-pgmacstbl))
                (t
                 (warn "Deletion affected more than 1 row; rolling back")
                 (pg-exec pgmacs--con "ROLLBACK TRANSACTION"))))))))

;; TODO: could handle a numeric arg prefix
(defun pgmacs--insert-row-empty ()
  "Insert an empty row into the PostgreSQL table at point."
  (interactive)
  (let* ((col-names (pg-columns pgmacs--con pgmacs--table))
         (nodefault-columns (list))
         (values (list))
         (value-types (list)))
    (cl-loop
     for col-name in col-names
     for col-id from 0
     for col-type = (aref pgmacs--column-type-names col-id)
     unless (pg-column-autogenerated-p pgmacs--con pgmacs--table col-name)
     do (let* ((val (pgmacs--read-value col-name col-type "Value for column %s (%s): " nil)))
          (push col-name nodefault-columns)
          (push val values)
          (push col-type value-types)))
    (let* ((placeholders (cl-loop for i from 1 to (length values)
                                  collect (format "$%d" i)))
           (target-cols (mapcar #'pg-escape-identifier nodefault-columns))
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
      (setq pgmacs--marked-rows (list))
      (pgmacs--display-table pgmacs--table))))


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
             (col-has-default (not (null (pg-column-autogenerated-p pgmacs--con pgmacs--table col-name)))))
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
      (setq pgmacs--marked-rows (list))
      (pgmacs--display-table pgmacs--table))))

(defun pgmacs--insert-row-widget (current-row)
  "Insert a new row of data into the current table after CURRENT-ROW.
Uses a widget-based buffer to prompt for new values.  Updates the
PostgreSQL database."
  (let* ((con pgmacs--con)
         (db-buffer pgmacs--db-buffer)
         (table pgmacs--table)
         (ce (pgcon-client-encoding con))
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
             (col-has-default (not (null (pg-column-autogenerated-p con table col-name)))))
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
                        (setq pgmacs--marked-rows (list))
                        (pgmacs--display-table table)))))
      (switch-to-buffer "*PGmacs insert row widget*")
      (erase-buffer)
      (remove-overlays)
      (kill-all-local-variables)
      (setq-local pgmacs--con con
                  pgmacs--db-buffer db-buffer
                  pgmacs--table table)
      (pgmacs-mode)
      (widget-insert (propertize (format "Insert row into table %s" table) 'face 'bold))
      (widget-insert "\n\n")
      (dolist (ecv editable-cols)
        (let ((name (aref ecv 0))
              (type (aref ecv 1)))
          (widget-insert (format "\n%7s (SQL type %s): " name type))
          (push (pgmacs--widget-for type nil) widgets)))
      (setq widgets (nreverse widgets))
      (widget-insert "\n\n")
      (widget-create 'push-button
                     :notify (lambda (&rest _ignore)
                               (let ((updates (mapcar #'widget-value widgets)))
                                 (kill-buffer (current-buffer))
                                 (funcall updater updates)))
                     "Insert row")
      (widget-insert "\n")
      (use-local-map widget-keymap)
      (widget-setup)
      (goto-char (point-min))
      (widget-forward 1))))

(defun pgmacs--copy-row (current-row)
  "Copy CURRENT-ROW to the PGmacs internal kill ring."
  (setq pgmacs--kill-ring (cons pgmacs--table current-row))
  (message "Row copied to PGmacs kill ring"))

;; Insert new row at current position based on content of our "kill ring".
(defun pgmacs--yank-row (_current-row)
  "Insert a new row into the current table after the current row.
The new row contents are based on the last copied row. Columns for which
a default SQL value is defined (such as a SERIAL type) will take the
default value instead of the last copied value.
Updates the PostgreSQL database."
  (unless pgmacs--kill-ring
    (user-error "PGmacs kill ring is empty"))
  (unless (eq (car pgmacs--kill-ring) pgmacs--table)
    (user-error "Can't paste into a different PostgreSQL table"))
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
               (col-has-default (not (null (pg-column-autogenerated-p pgmacs--con pgmacs--table col-name)))))
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
      (setq pgmacs--marked-rows (list))
      ;; Redisplay the table, but attempt to put point on the new row
      (let ((pos (point)))
        (pgmacs--display-table pgmacs--table)
        (when (< pos (point-max))
          (goto-char pos)
          (recenter))))))

;; This SQL query adapted from https://stackoverflow.com/a/20537829
(defun pgmacs--table-primary-keys (con table)
  "Return the columns active as PRIMARY KEY in TABLE.
Uses PostgreSQL connection CON."
  (let* ((schema (if (pg-qualified-name-p table)
                     (pg-qualified-name-schema table)
                   "public"))
         (tname (if (pg-qualified-name-p table)
                    (pg-qualified-name-name table)
                  table))
         (sql "SELECT a.attname
               FROM pg_catalog.pg_index idx
               JOIN pg_catalog.pg_class c ON c.oid = idx.indrelid
               JOIN pg_catalog.pg_attribute a ON a.attrelid = c.oid AND a.attnum = ANY(idx.indkey)
               JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
               WHERE relname = $1 AND nspname = $2 AND indisprimary")
         (argument-types (list "text" "text"))
         (params `((,tname . "text") (,schema . "text")))
         (ps-name (pg-ensure-prepared-statement con "QRY-tbl-primary-keys" sql argument-types))
         (res (pg-fetch-prepared con ps-name params)))
    (mapcar #'cl-first (pg-result res :tuples))))

(defun pgmacs--table-indexes (con table)
  "Return details of the indexes present on TABLE.
Uses PostgreSQL connection CON."
  (let* ((schema (if (pg-qualified-name-p table)
                     (pg-qualified-name-schema table)
                   "public"))
         (tname (if (pg-qualified-name-p table)
                    (pg-qualified-name-name table)
                         table))
         (sql "SELECT
                  c.relname AS index_name,
                  i.indisunique AS is_unique,
                  i.indisprimary AS is_primary,
                  i.indisclustered AS is_clustered,
                  i.indisvalid AS is_valid,
                  pg_catalog.pg_get_indexdef(i.indexrelid, 0, true) AS index_definition,
                  am.amname AS index_type,
                  ARRAY_AGG(a.attname ORDER BY x.ordinality) AS indexed_columns
               FROM
                  pg_class c
                  JOIN pg_index i ON c.oid = i.indexrelid
                  JOIN pg_class t ON t.oid = i.indrelid
                  JOIN pg_namespace n ON n.oid = t.relnamespace
                  JOIN pg_am am ON c.relam = am.oid
                  JOIN LATERAL unnest(i.indkey) WITH ORDINALITY AS x(attnum, ordinality)
                      ON TRUE
                  JOIN pg_attribute a ON a.attrelid = t.oid AND a.attnum = x.attnum
               WHERE
                  n.nspname = $1
                  AND t.relname = $2
               GROUP BY
                  n.nspname, t.relname, c.relname, i.indisunique, i.indisprimary, i.indisclustered, i.indisvalid, i.indexrelid, am.amname
               ORDER BY c.relname")
         (argument-types (list "text" "text"))
         (params `((,schema . "text") (,tname . "text")))
         (ps-name (pg-ensure-prepared-statement con "QRY-table-indexes" sql argument-types))
         (res (pg-fetch-prepared con ps-name params)))
    (pg-result res :tuples)))

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
         (argument-types (list "text" "text" "text"))
         (params `((,schema . "text") (,tname . "text") (,column . "text")))
         (ps-name (pg-ensure-prepared-statement con "QRY-column-nullable" sql argument-types))
         (res (pg-fetch-prepared con ps-name params)))
    (null (pg-result res :tuples))))

;; Function pgmacs--column-info uses some moderately complex SQL queries to determine the
;; constraints of a column. These queries are called once per column for a row-list buffer. To avoid
;; redundant processing by PostgreSQL in parsing and preparing a query plan for these queries, we
;; use PostgreSQL prepared statements via the `pg-ensure-prepared' function.
(defun pgmacs--column-info (con table column)
  "Return a hashtable containing metainformation on COLUMN in TABLE.
The metainformation includes the type name, whether the column is a PRIMARY KEY,
whether it is affected by constraints such as UNIQUE.  Information is retrieved
over the PostgreSQL connection CON."
  (let* ((schema (if (pg-qualified-name-p table)
                     (pg-qualified-name-schema table)
                   "public"))
         (tname (if (pg-qualified-name-p table)
                    (pg-qualified-name-name table)
                  table))
         (sql "SELECT tc.constraint_type, tc.constraint_name
               FROM information_schema.table_constraints tc
               JOIN information_schema.constraint_column_usage AS ccu USING (constraint_schema, constraint_name)
               JOIN information_schema.columns AS c ON c.table_schema = tc.constraint_schema
               AND tc.table_name = c.table_name
               AND ccu.column_name = c.column_name
               WHERE tc.constraint_schema = $1
                 AND tc.table_name = $2
                 AND c.column_name = $3
                 AND tc.constraint_type != 'FOREIGN KEY'")
         (argument-types (list "text" "text" "text"))
         (params `((,schema . "text") (,tname . "text") (,column . "text")))
         (ps-name (pg-ensure-prepared-statement con "QRY-check-constraints" sql argument-types))
         (res (pg-fetch-prepared con ps-name params))
         (check-constraints (pg-result res :tuples))
         (sql "SELECT
                 ccu.table_schema AS foreign_table_schema,
                 ccu.table_name AS foreign_table_name,
                 ccu.column_name AS foreign_column_name
               FROM information_schema.table_constraints tc
               JOIN information_schema.constraint_column_usage AS ccu
                 USING (constraint_schema, constraint_name)
               JOIN information_schema.key_column_usage AS kcu
                 ON kcu.constraint_name = tc.constraint_name
                 AND kcu.table_schema = tc.table_schema
               WHERE tc.constraint_type = 'FOREIGN KEY'
                 AND tc.constraint_schema = $1
                 AND tc.table_name = $2
                 AND kcu.column_name = $3")
         ;; this query has the same arguments (and argument types) as that above
         (ps-name (pg-ensure-prepared-statement con "QRY-references-constraints" sql argument-types))
         (res (pg-fetch-prepared con ps-name params))
         (references-constraints (pg-result res :tuples))
         (sql "SELECT character_maximum_length FROM information_schema.columns
                WHERE table_schema=$1 AND table_name=$2 AND column_name=$3")
         (ps-name (pg-ensure-prepared-statement con "QRY-maxlen" sql argument-types))
         (res (pg-fetch-prepared con ps-name params))
         (maxlen (pg-result res :tuple 0))
         ;; TODO perhaps want to include GENERATED ALWAYS information in here
         ;; the generation_expression column of information.schema.columns
         (defaults (pg-column-default con table column))
         (sql (format "SELECT %s FROM %s LIMIT 0"
                      (pg-escape-identifier column)
                      (pg-escape-identifier table)))
         (res (pg-exec con sql))
         (oid (cadar (pg-result res :attributes)))
         (type-name (pg-lookup-type-name con oid))
         (column-info (make-hash-table :test #'equal)))
    (puthash "TYPE" type-name column-info)
    (dolist (c check-constraints)
      (cond ((string= "CHECK" (cl-first c))
             (let* ((sql "SELECT check_clause FROM information_schema.check_constraints
                          WHERE constraint_schema=$1 and constraint_name=$2")
                    (res (pg-exec-prepared con sql `((,schema . "text") (,(cl-second c) . "text"))))
                    (clauses (pg-result res :tuple 0)))
               (puthash (cl-first c) (format "%s %s" (cl-second c) (cl-first clauses)) column-info)))
            (t
             (puthash (cl-first c) (cl-second c) column-info))))
    (dolist (c references-constraints)
      (let ((sqn (make-pg-qualified-name :schema (cl-first c) :name (cl-second c))))
        (puthash "REFERENCES" (list sqn (cl-third c)) column-info)))
    (when (pgmacs--column-nullable-p con table column)
      (puthash "NOT NULL" nil column-info))
    (when (cl-first maxlen)
      (puthash "maxlen" (cl-first maxlen) column-info))
    (when defaults
      (puthash "DEFAULT" defaults column-info))
    column-info))

;; Format the column-info hashtable as a string
(defun pgmacs--format-column-info (column-info)
  (let ((items (list)))
    (maphash (lambda (k v)
               (cond ((string= "TYPE" k) nil)
                     ((string= "REFERENCES" k)
                      (push (format "REFERENCES %s(%s)"
                                    (pgmacs--display-identifier (cl-first v))
                                    (cl-second v))
                            items))
                     (t
                      (push (if v (format "%s %s" k v) k) items))))
             column-info)
    ;; We want the type to appear first in the column metainformation
    (let ((type (gethash "TYPE" column-info)))
      (when type
        (push type items)))
    (string-join items ", ")))

;; We could first check whether the row_security_function() is implemented in this PostgresQL
;; version by querying the pg_proc table for a function with proname='row_security_active', but it's
;; easier to ignore errors.
(defun pgmacs--row-security-active (con table)
  "Is row-level access control active for PostgreSQL TABLE?
Uses PostgreSQL connection CON."
  (let* ((sql "SELECT row_security_active($1)")
         (tid (pg-escape-identifier table))
         (res (ignore-errors
                (pg-exec-prepared con sql `((,tid . "text"))))))
    (when res (cl-first (pg-result res :tuple 0)))))


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
;;
;; We could perhaps fill in the row count column in a lazy manner to improve query speed.
(defun pgmacs--list-tables ()
  "Return a list of table-names and associated metadata for the current database.
Table names are schema-qualified if the schema is non-default."
  (let ((entries (list)))
    (dolist (table (pg-tables pgmacs--con))
      (let* ((tid (pg-escape-identifier table))
             (sql (format "SELECT
                            COUNT(*),
                            pg_catalog.pg_size_pretty(pg_catalog.pg_total_relation_size($1)),
                            obj_description($1::regclass::oid, 'pg_class')
                           FROM %s"
                          tid))
             (res (pg-exec-prepared pgmacs--con sql `((,tid . "text"))))
             (tuple (pg-result res :tuple 0))
             (rows (cl-first tuple))
             (size (cl-second tuple))
             ;; We could use function `pg-table-comment', but that would imply an additional SQL
             ;; query and this function is speed critical.
             (comment (cl-third tuple))
             (owner (pg-table-owner pgmacs--con table)))
        (push (list table rows size owner (or comment "")) entries)))
    entries))

;; Used to display only the first line of a table cell.
(defun pgmacs--truncate-multiline (string)
  (let ((pos (or (cl-position ?\C-m string)
                 (cl-position ?\C-j string))))
    (if pos (concat (substring string 0 pos) (truncate-string-ellipsis))
      string)))

(defun pgmacs--make-column-displayer (echo-text column-metainfo)
  "Return a display function which echos ECHO-TEXT in minibuffer."
  (lambda (fvalue max-width _table)
    (let* ((first-line (pgmacs--truncate-multiline fvalue))
           (truncated (if (> (string-pixel-width first-line) max-width)
                          ;; TODO could include the ellipsis here
                          (pgmacstbl--limit-string first-line max-width)
                        first-line))
           (face (cond
                  ;; For a row-list buffer created by pgmacs-show-result, we have no column-metainfo
                  ((null column-metainfo)
                   'pgmacs-table-data)
                  ((gethash "REFERENCES" column-metainfo)
                   'pgmacs-column-foreign-key)
                  ((gethash "PRIMARY KEY" column-metainfo)
                   'pgmacs-column-primary-key)
                  (t 'pgmacs-table-data))))
      (propertize truncated
                  'face face
                  'help-echo echo-text
                  'pgmacs--column-info column-metainfo))))

(defun pgmacs--table-to-csv (&rest _ignore)
  "Dump the current PostgreSQL table in CSV format into an Emacs buffer."
  (let* ((con pgmacs--con)
         (db-buffer pgmacs--db-buffer)
         (table pgmacs--table)
         (t-id (pg-escape-identifier table))
         (t-pretty (pgmacs--display-identifier table))
         (buf (get-buffer-create (format "*PostgreSQL CSV for %s*" t-pretty)))
         (sql (format "COPY %s TO STDOUT WITH (FORMAT CSV)" t-id)))
    (pop-to-buffer buf)
    (erase-buffer)
    (remove-overlays)
    (kill-all-local-variables)
    (setq-local pgmacs--db-buffer db-buffer)
    (pgmacs-transient-mode)
    (require 'csv-mode nil t)
    (when (fboundp 'csv-mode)
      (csv-mode))
    (pg-copy-to-buffer con sql buf)
    (goto-char (point-min))))

(defun pgmacs--add-primary-key (&rest _ignore)
  "Add a PRIMARY KEY to the current PostgreSQL table."
  (let ((pk (pgmacs--table-primary-keys pgmacs--con pgmacs--table)))
    (when pk
      (user-error "Table %s already has a primary key %s" (pgmacs--display-identifier pgmacs--table) pk)))
  (cl-flet ((exists (name) (cl-find name (pg-columns pgmacs--con pgmacs--table) :test #'string=)))
    (let* ((colname (or (cl-find-if-not #'exists (list "id" "idpk" "idcol" "pk" "_id" "newpk"))
                        (error "Can't autogenerate a name for primary key")))
           (sql (format "ALTER TABLE %s ADD COLUMN %s BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY"
                        (pg-escape-identifier pgmacs--table)
                        (pg-escape-identifier colname))))
      (when (y-or-n-p (format "Really run SQL '%s'?" sql))
        (let ((res (pg-exec pgmacs--con sql)))
          (pgmacs--notify "%s" (pg-result res :status))))))
  (pgmacs--display-table pgmacs--table))

(defun pgmacs--display-procedures (&rest _ignore)
  "Open a buffer displaying the FUNCTIONs and PROCEDURES defined in this database."
  (let* ((db-buffer pgmacs--db-buffer)
         (con pgmacs--con)
         (sql "SELECT n.nspname AS schema_name,
                      p.proname AS specific_name,
                      CASE p.prokind
                           when 'f' then 'FUNCTION'
                           when 'p' then 'PROCEDURE'
                           when 'a' then 'AGGREGATE'
                           when 'w' then 'WINDOW'
                      END AS KIND,
                      l.lanname as language,
                      CASE WHEN l.lanname = 'internal' THEN p.prosrc
                           ELSE pg_catalog.pg_get_functiondef(p.oid)
                           END AS DEFINITION,
                      pg_catalog.pg_get_function_arguments(p.oid) AS arguments,
                      t.typname AS return_type
               FROM pg_catalog.pg_proc p
               LEFT JOIN pg_catalog.pg_namespace n ON p.pronamespace = n.oid
               LEFT JOIN pg_catalog.pg_language l ON p.prolang = l.oid
               LEFT JOIN pg_catalog.pg_type t ON t.oid = p.prorettype
               WHERE n.nspname NOT IN ('pg_catalog', 'information_schema')
               ORDER BY schema_name, specific_name")
         (ps-name (pg-ensure-prepared-statement con "QRY-list-procedures" sql nil))
         (res (pg-fetch-prepared con ps-name nil))
         (buf (get-buffer-create "*PGmacs procedures*")))
    (pop-to-buffer buf)
    (erase-buffer)
    (remove-overlays)
    (kill-all-local-variables)
    (setq-local pgmacs--con con
                pgmacs--db-buffer db-buffer
                buffer-read-only t
                truncate-lines t)
    (pgmacs-mode)
    (let ((inhibit-read-only t))
      (insert (propertize "PostgreSQL functions and procedures" 'face 'bold))
      (insert "\n\n")
      (pgmacs--show-pgresult buf res))))

(defun pgmacs--display-running-queries (&rest _ignore)
  "Display the list of queries running in PostgreSQL.
Opens a dedicated buffer if the query list is not empty."
  (let* ((db-buffer pgmacs--db-buffer)
         (con pgmacs--con)
         (sql "SELECT pid, age(clock_timestamp(), query_start) AS duration, usename, query, state
               FROM pg_catalog.pg_stat_activity
               WHERE state != 'idle' AND query NOT ILIKE '%pg_stat_activity%'
               ORDER BY query_start DESC")
         (ps-name (pg-ensure-prepared-statement con "QRY-running-queries" sql nil))
         (res (pg-fetch-prepared con ps-name nil))
         (tuples (pg-result res :tuples)))
    (cond ((null tuples)
           (pgmacs--notify "No running queries" nil))
          (t
           (let ((buf (get-buffer-create "*PostgreSQL running queries*")))
             (pop-to-buffer buf)
             (erase-buffer)
             (remove-overlays)
             (kill-all-local-variables)
             (setq-local pgmacs--con con
                         pgmacs--db-buffer db-buffer
                         buffer-read-only t
                         truncate-lines t)
             (pgmacs-mode)
             (let ((inhibit-read-only t))
               (insert (propertize "Queries running in this PostgreSQL backend" 'face 'bold))
               (insert "\n\n")
               (pgmacs--show-pgresult buf res)))))))

(defun pgmacs--run-analyze (&rest _ignore)
  "Run ANALYZE on the current PostgreSQL table."
  (let* ((sql (format "ANALYZE %s" (pg-escape-identifier pgmacs--table)))
         (res (pg-exec pgmacs--con sql)))
    (pgmacs--notify "%s" (pg-result res :status))))

(defun pgmacs--run-count (&rest _ignore)
  "Count the number of rows in the current PostgreSQL table."
  (let* ((sql (format "SELECT COUNT(*) FROM %s" (pg-escape-identifier pgmacs--table)))
         (res (pg-exec pgmacs--con sql))
         (count (cl-first (pg-result res :tuple 0))))
    (pgmacs--notify "Table %s has %s row%s"
                    pgmacs--table
                    count
                    (if (> count 1) "s" ""))))

(defun pgmacs--find-completable-symbol ()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (buffer-substring-no-properties (car bounds) (cdr bounds)))))

;; See elisp-completion-at-point.
(defun pgmacs--completion-at-point ()
  (let ((completion-ignore-case t)
	(pattern (pgmacs--find-completable-symbol))
	beg)
    (when pattern
      (save-excursion
        ;; Avoid end-of-buffer error.
        (goto-char (+ (point) (length pattern) -1))
        (when (search-backward pattern nil t)
          (setq beg (point))
          (forward-char (length pattern))
          (list beg (point) pgmacs--completions :exclusive 'no))))))

(defvar pgmacs--where-filter-history nil)

;; TODO: can we font-lock with sql-mode-postgres-font-lock-keywords ?
(defun pgmacs--read-sql-minibuffer (prompt completions)
  ;; See function read--expression in simple.el
  (minibuffer-with-setup-hook
      (lambda ()
        (set-syntax-table sql-mode-syntax-table)
        ;; cf. minibuffer-local-completion-map
        (local-set-key (kbd "<tab>") #'completion-at-point)
        (setq-local pgmacs--completions completions)
        (add-hook 'completion-at-point-functions
                  #'pgmacs--completion-at-point nil t))
    ;; (minibuffer-message "(without the WHERE keyword)")
    (let ((completion-styles '(basic flex)))
      (read-from-minibuffer prompt nil nil nil 'pgmacs--where-filter-history))))

;; The list of column names in the current table. Note that this function needs to be called in the
;; PGmacs buffer (where the text properties it consults are set), and not in the minibuffer. If we
;; implement fuzzy matching, we should escape the column names with pg-escape-identifiers, because a
;; case-sensitive SQL identifier will need quoting to be recognized by PostgreSQL.
(defun pgmacs--completion-table ()
  (save-excursion
    (pgmacstbl-beginning-of-table)
    (let* ((tbl (pgmacstbl-current-table))
           (cols (pgmacstbl-columns tbl)))
      (mapcar #'pgmacstbl-column-name cols))))

;; Bound to "W" in a row-list buffer.
(defun pgmacs--add-where-filter (&rest _ignore)
  (interactive)
  (unless (zerop pgmacs--offset)
    (message "Resetting table OFFSET")
    (sit-for 0.5))
  (setq pgmacs--offset 0)
  (let ((filter (pgmacs--read-sql-minibuffer "WHERE: " (pgmacs--completion-table))))
    (cond ((zerop (length filter))
           (message "Cancelling WHERE filter")
           (setq pgmacs--marked-rows (list))
           (pgmacs--display-table pgmacs--table))
          (t
           (message "Using WHERE filter %s" filter)
           (setq pgmacs--marked-rows (list))
           (pgmacs--display-table pgmacs--table :where-filter filter)))))

(defun pgmacs--paginated-next (&rest _ignore)
  "Move to the next page of the paginated PostgreSQL table."
  (interactive)
  (cl-incf pgmacs--offset pgmacs-row-limit)
  (setq pgmacs--marked-rows (list))
  (pgmacs--display-table pgmacs--table))

(defun pgmacs--paginated-prev (&rest _ignore)
  "Move to the previous page of the paginated PostgreSQL table."
  (interactive)
  (cl-decf pgmacs--offset pgmacs-row-limit)
  (setq pgmacs--marked-rows (list))
  (pgmacs--display-table pgmacs--table))


(defun pgmacs--row-list-help (&rest _ignore)
  "Open a buffer describing keybindings in a row-list buffer."
  (interactive)
  (pop-to-buffer "*PGmacs row-list help*")
  (erase-buffer)
  (buffer-disable-undo)
  (help-mode)
  (cl-flet ((shw (key msg)
              (insert (propertize (format "%12s" key) 'face '(:foreground "blue")))
              (insert (propertize " → " 'face '(:foreground "gray")))
              (insert msg "\n")))
    (let ((inhibit-read-only t))
      (shw "v" "Display the value at point in a dedicated buffer")
      (shw "RET" "Edit the value at point in the minibuffer")
      (shw "w" "Edit the value at point in a widget-based buffer")
      (shw "W" "Specify a WHERE filter to apply to displayed rows")
      (shw "<backspace>" "Delete the row at point")
      (shw "DEL" "Delete the row at point")
      (shw "+" "Insert a new row, prompting for new values in minibuffer")
      (shw "i" "Insert a new row, prompting for new values in a dedicated buffer")
      (shw "k" "Copy the row at point")
      (shw "y" "Yank the previously copied row and insert into the table")
      (shw "j" "Copy the current row to the kill-ring in JSON format")
      (shw "d" "Mark the current row for deletion")
      (shw "u" "Unmark the current row (deselect for deletion)")
      (shw "U" "Unmark all rows (deselect all for deletion)")
      (shw "x" "Delete marked rows")
      (shw "R" "Rename the current column")
      (shw "!" "Run a filter-like shell command with current cell value as input")
      (shw "&" "Run a program with the value of current cell as first argument")
      (shw "M-u" "Upcase the value of the current cell")
      (shw "M-l" "Downcase the value of the current cell")
      (shw "M-c" "Capitalize the value of the current cell")
      (shw "n" "Next page of output (if table contents are paginated)")
      (shw "p" "Previous page of output (if table contents are paginated)")
      (shw "e" "New buffer with output from SQL query")
      (shw "E" "Run SQL from a buffer and display the output")
      (shw "<number>" "Move point to nth column")
      (shw "S" "Run SchemaSpy on the current table and display the SVG output")
      (shw "<" "Move point to the first row in the table")
      (shw ">" "Move point to the last row in the table")
      (shw "{" "Shrink the horizontal space used by the current column")
      (shw "}" "Grow the horizontal space used by the current column")
      (shw "o" "Prompt for a table name and open a new buffer displaying that table's data")
      (shw "r" "Redraw the table without refetching data from PostgreSQL")
      (shw "g" "Redraw the table (refetches data from PostgreSQL)")
      (shw "T" "Switch to the main table-list buffer for this database")
      (shw "q" "Bury this buffer")
      (shrink-window-if-larger-than-buffer)
      (goto-char (point-min)))))

;; Bound to "d" in a row-list buffer.
;;
;; How to add a face attribute (such as :background "red") to the entire line? It won't work to look
;; at the face at one point in the line and simply add (or replace) a component to that, because
;; some columns are using special face features such as bold (primary key) and blue foreground
;; (foreign key references). So we need to be "adding" an attribute, using add-face-text-property.
;; But deleting that later is tricky...
(cl-defun pgmacs--row-list-mark-row (primary-keys)
  "Mark the current row for deletion."
  (when (null primary-keys)
    (message "Can't delete from a table that has no PRIMARY KEY")
    (cl-return-from pgmacs--row-list-mark-row))
  ;; Note: this line number is zero-based
  (when-let ((line-number (get-text-property (point) 'pgmacstbl-line-number)))
    (cl-pushnew line-number pgmacs--marked-rows)
    (let* ((table (pgmacstbl-current-table))
           (buffer-read-only nil))
      (pgmacstbl-mark-row table line-number :marked-for-deletion)
      (add-face-text-property (pos-bol) (pos-eol) '(:background "red"))
      (forward-line 1))))

;; Bound to "u" in a row-list buffer.
(cl-defun pgmacs--row-list-unmark-row (&rest _ignore)
  "Unmark the current row for deletion."
  ;; Note: this line number is zero-based
  (when-let ((line-number (get-text-property (point) 'pgmacstbl-line-number)))
    (cond ((member line-number pgmacs--marked-rows)
           (setq pgmacs--marked-rows (cl-delete line-number pgmacs--marked-rows))
           (let* ((table (pgmacstbl-current-table))
                  (buffer-read-only nil))
             (pgmacstbl-unmark-row table line-number)
             ;; We are redrawing the whole table here instead of only redrawing the current line.
             ;; This appears wasteful but seems necessary: when we mark a row in
             ;; `pgmacs--row-list-mark-row', we can use the `add-face-text-property' function to
             ;; add a face component (such as a particular background color) to the whole line,
             ;; preserving other aspects of the face (eg. bolds and foreground colors on certain
             ;; columns). However, there is no `remove-face-text-property' that reverses the
             ;; addition, so we need to recalculate all the faces from scratch.
             (pgmacs--redraw-pgmacstbl)
             (forward-line 1)))
          (t
           (message "Current row is not marked")))))

;; Bound to "U" in a row-list buffer.
(cl-defun pgmacs--row-list-unmark-all (&rest _ignore)
  "Unmark all rows (deselect them all for deletion)."
  (let* ((table (pgmacstbl-current-table))
         (buffer-read-only nil))
    (dolist (line-number pgmacs--marked-rows)
      (pgmacstbl-unmark-row table line-number))
    (setq pgmacs--marked-rows (list)))
  (pgmacs--redraw-pgmacstbl))

;; Bound to "x" in a row-list buffer.
(cl-defun pgmacs--row-list-delete-marked (primary-keys)
  "Delete rows in the current table marked for deletion using `d'.
Deletion is only possible for tables with a (possibly multicolumn) primary key,
specied by PRIMARY-KEYS."
  (when (null pgmacs--marked-rows)
    (message "No rows are marked for deletion")
    (cl-return-from pgmacs--row-list-delete-marked))
  (when (null primary-keys)
    (user-error "Can't delete from a table that has no PRIMARY KEY"))
  (when (y-or-n-p (format "Really delete %d PostgreSQL rows?" (length pgmacs--marked-rows)))
    (let* ((pgmacstbl (pgmacstbl-current-table))
           (cols (pgmacstbl-columns pgmacstbl))
           (counter 0)
           (where-clauses-all (list))
           (where-values-all (list)))
      (dolist (line-number pgmacs--marked-rows)
        (let ((where-clauses-line (list))
              (where-values-line (list)))
          (dolist (pk primary-keys)
            (let* ((col-name (cl-position pk cols :key #'pgmacstbl-column-name :test #'string=))
                   (col-type (and col-name (aref pgmacs--column-type-names col-name)))
                   (row (nth line-number (pgmacstbl-objects pgmacstbl)))
                   (value (and col-name (nth col-name row))))
              (unless value
                (error "Can't find value for primary key %s" pk))
              (push (format "%s = $%d" (pg-escape-identifier pk) (cl-incf counter)) where-clauses-line)
              (push (cons value col-type) where-values-line)))
          (push (concat "(" (string-join (reverse where-clauses-line) " AND ") ")") where-clauses-all)
          (setf where-values-all (append where-values-all (reverse where-values-line)))))
      ;;  WHERE (fooid=42 AND foobles='bizzles') OR (fooid=33 AND fooobles='qdsfsd') OR (...)
      (let* ((sql (format "DELETE FROM %s WHERE %s"
                          (pg-escape-identifier pgmacs--table)
                          (string-join where-clauses-all " OR ")))
             (_ (pg-exec pgmacs--con "START TRANSACTION"))
             (res (pg-exec-prepared pgmacs--con sql where-values-all))
             (status (pg-result res :status)))
        (pgmacs--notify "%s" status)
        (unless (string= "DELETE " (substring status 0 7))
          (error "Unexpected status %s for PostgreSQL DELETE command" status))
        (let ((rows-affected (cl-parse-integer (substring status 7))))
          (cond ((eql 0 rows-affected)
                 (warn "Could not delete PostgreSQL rows")
                 (pg-exec pgmacs--con "COMMIT TRANSACTION"))
                ((eql (length pgmacs--marked-rows) rows-affected)
                 (pg-exec pgmacs--con "COMMIT TRANSACTION")
                 (dolist (line-number pgmacs--marked-rows)
                   (let ((row (nth line-number (pgmacstbl-objects pgmacstbl))))
                     (pgmacstbl-remove-object pgmacstbl row)))
                 (pgmacs--redraw-pgmacstbl))
                (t
                 (warn "Deletion affected more than 1 row; rolling back")
                 (pg-exec pgmacs--con "ROLLBACK TRANSACTION"))))))
    (setq pgmacs--marked-rows (list))))

(defun pgmacs--row-list-rename-column (&rest _ignore)
  "Rename the current PostgreSQL column."
  (let* ((col-id (pgmacstbl-current-column))
         (cols (pgmacstbl-columns (pgmacstbl-current-table)))
         (col (nth col-id cols))
         (col-name (pgmacstbl-column-name col))
         (new (read-string (format "Rename column %s to: " col-name)))
         (sql (format "ALTER TABLE %s RENAME %s TO %s"
                      (pg-escape-identifier pgmacs--table)
                      (pg-escape-identifier col-name)
                      (pg-escape-identifier new)))
         (res (pg-exec pgmacs--con sql)))
    (pgmacs--notify "%s" (pg-result res :status))
    (pgmacs--display-table pgmacs--table)))

;; Select row-count values from table "around" (ordered by pk) the row where pk=value.
;; center-on is a list of the form (pk pk-value pk-type)
(defun pgmacs--select-rows-around (con table-name-escaped center-on row-count)
  (let* ((pk-id (pg-escape-identifier (cl-first center-on)))
         (pk-val (cl-second center-on))
         (pk-type (cl-third center-on))
         (sql (format "WITH target_row AS (SELECT * FROM %s WHERE %s = $1),
                       rows_before AS (SELECT * FROM %s WHERE %s < $1 ORDER BY %s DESC LIMIT $2),
                       rows_after AS (SELECT * FROM %s WHERE %s > $1 ORDER BY %s ASC LIMIT $2)
                       SELECT * FROM rows_before
                       UNION ALL
                       SELECT * FROM target_row
                       UNION ALL
                       SELECT * FROM rows_after
                       ORDER BY %s"
                      table-name-escaped pk-id
                      table-name-escaped pk-id pk-id
                      table-name-escaped pk-id pk-id
                      pk-id))
         (params `((,pk-val . ,pk-type) (,(/ row-count 2) . "int4"))))
    (pg-exec-prepared con sql params)))

;; Used to retrieve rows in a row-list buffer.
(defun pgmacs--select-rows-offset (con table-name-escaped offset row-count)
  (let ((sql (format "SELECT * FROM %s OFFSET %s" table-name-escaped offset)))
    (pg-exec-prepared con sql (list) :max-rows row-count)))

(defun pgmacs--select-rows-where (con table-name-escaped where-filter row-count)
  (when (cl-search ";" where-filter)
    (user-error "WHERE filter must not contain end-of-statement marker ';'"))
  (let ((sql (format "SELECT * FROM %s WHERE %s" table-name-escaped where-filter)))
    (pg-exec-prepared con sql (list) :max-rows row-count)))


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

(cl-defun pgmacs--display-table (table &key center-on where-filter)
  "Open a row-list buffer to display TABLE in PGmacs.
TABLE may be specified as a string or as a schema-qualified pg-qualified-name
object.

Keyword argument CENTER-ON of the form (pk-name pk-value pk-type)
specifies the name, value and type of a primary key which we wish
to have centered in the display (rows will be shown for values
smaller than and larger than this value, in the limit of
pgmacs-row-limit.

Keyword argument WHERE-FILTER is an SQL WHERE clause which filters the
rows to display in the table.

The CENTER-ON and WHERE-FILTER arguments are mutually exclusive."
  (when (and center-on where-filter)
    (user-error "CENTER-ON and WHERE-FILTER arguments are mutually exclusive"))
  (let* ((con pgmacs--con)
         (db-buffer pgmacs--db-buffer)
         (t-id (pg-escape-identifier table))
         (t-pretty (pgmacs--display-identifier table)))
    (pop-to-buffer-same-window (format "*PostgreSQL %s %s*" (pgcon-dbname con) t-pretty))
    (setq-local pgmacs--db-buffer db-buffer
                ;; We need to save a possible WHERE filter, because if the user triggers a
                ;; refetch+redraw of the table, we need to retain the filter.
                pgmacs--where-filter where-filter)
    (pgmacs--start-progress-reporter "Retrieving data from PostgreSQL")
    ;; Place some initial content in the buffer early up.
    (let* ((inhibit-read-only t)
           (owner (pg-table-owner con table))
           (maybe-icon (pgmacs--maybe-svg-icon #'pgmacs--svg-icon-user))
           (owner-displayed (concat maybe-icon owner))
           (header (format "PostgreSQL table %s, owned by %s\n" t-pretty owner-displayed)))
      (erase-buffer)
      (insert (propertize header 'face 'bold)))
    (let* ((primary-keys (pgmacs--table-primary-keys con table))
           (comment (pg-table-comment con table))
           (indexes (pgmacs--table-indexes con table))
           (offset (or pgmacs--offset 0))
           (res (cond (center-on
                       (pgmacs--select-rows-around con t-id center-on pgmacs-row-limit))
                      (where-filter
                       (pgmacs--select-rows-where con t-id where-filter pgmacs-row-limit))
                      (t
                       (pgmacs--select-rows-offset con t-id offset pgmacs-row-limit))))
           (rows (pg-result res :tuples))
           (column-names (mapcar #'cl-first (pg-result res :attributes)))
           (column-type-oids (mapcar #'cl-second (pg-result res :attributes)))
           (column-type-names (mapcar (lambda (oid) (pg-lookup-type-name con oid)) column-type-oids))
           (column-alignment (mapcar #'pgmacs--alignment-for column-type-names))
           (column-info
            (let ((ht (make-hash-table :test #'equal)))
              (dolist (c column-names)
                (puthash c (pgmacs--column-info con table c) ht))
              ht))
           (column-meta (cl-loop
                         for c in column-names
                         for ci = (gethash c column-info)
                         collect (pgmacs--format-column-info ci)))
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
                     for dpy = (or (pgmacs--lookup-column-displayer table name)
                                   (pgmacs--make-column-displayer meta (gethash name column-info)))
                     collect (make-pgmacstbl-column
                              :name (propertize name
                                                'face 'pgmacs-table-header
                                                'help-echo meta)
                              :align align
                              :min-width (1+ (max w (length name)))
                              :max-width pgmacs-max-column-width
                              :formatter fmt
                              :displayer dpy)))
           (inhibit-read-only t)
           (pgmacstbl (make-pgmacstbl
                       :insert nil
                       :use-header-line nil
                       :face 'pgmacs-table-data
                       :columns columns
                       :row-colors pgmacs-row-colors
                       :objects rows
                       ;; same syntax for keys as keymap-set
                       ;; TODO: the primary-keys could perhaps be saved as a text property on the table?
                       :actions `("RET" (lambda (row) (pgmacs--table-list-dwim row ',primary-keys))
                                  "w" (lambda (row) (pgmacs--edit-value-widget row ',primary-keys))
                                  "!" (lambda (row) (pgmacs--shell-command-on-value row ',primary-keys))
                                  "&" pgmacs--async-command-on-value
                                  "M-u" (lambda (row) (pgmacs--upcase-value row ',primary-keys))
                                  "M-l" (lambda (row) (pgmacs--downcase-value row ',primary-keys))
                                  "M-c" (lambda (row) (pgmacs--capitalize-value row ',primary-keys))
                                  "v" pgmacs--view-value
                                  "<delete>" (lambda (row) (pgmacs--delete-row row ',primary-keys))
                                  "<deletechar>" (lambda (row) (pgmacs--delete-row row ',primary-keys))
                                  "<backspace>" (lambda (row) (pgmacs--delete-row row ',primary-keys))
                                  "DEL" (lambda (row) (pgmacs--delete-row row ',primary-keys))
                                  "TAB" (lambda (_row) (pgmacstbl-next-column))
                                  "<backtab>" (lambda (_row) (pgmacstbl-previous-column))
                                  "R" pgmacs--row-list-rename-column
                                  "h" pgmacs--row-list-help
                                  "?" pgmacs--row-list-help
                                  "o" pgmacs-open-table
                                  "+" pgmacs--insert-row
                                  "i" pgmacs--insert-row-widget
                                  "k" pgmacs--copy-row
                                  "y" pgmacs--yank-row
                                  "e" pgmacs-run-sql
                                  "E" pgmacs-run-buffer-sql
                                  "S" pgmacs--schemaspy-table
                                  "W" pgmacs--add-where-filter
                                  "=" pgmacs--shrink-columns
                                  ;; pgmacs--redraw-pgmacstbl does not refetch data from PostgreSQL;
                                  ;; pgmacs--row-list-redraw does refetch.
                                  "r" pgmacs--redraw-pgmacstbl
                                  "g" pgmacs--row-list-redraw
                                  "j" pgmacs--row-as-json
                                  "d" (lambda (&rest _ignore) (pgmacs--row-list-mark-row ',primary-keys))
                                  "u" pgmacs--row-list-unmark-row
                                  "U" pgmacs--row-list-unmark-all
                                  "x" (lambda (&rest _ignored) (pgmacs--row-list-delete-marked ',primary-keys))
                                  ;; "n" and "p" are bound when table is paginated to next/prev page
                                  "<" (lambda (&rest _ignored)
                                        (text-property-search-backward 'pgmacstbl)
                                        (next-line))
                                  ">" (lambda (&rest _ignored)
                                        (text-property-search-forward 'pgmacstbl)
                                        (previous-line))
                                  "0" (lambda (&rest _ignored) (pgmacstbl-goto-column 0))
                                  "1" (lambda (&rest _ignored) (pgmacstbl-goto-column 1))
                                  "2" (lambda (&rest _ignored) (pgmacstbl-goto-column 2))
                                  "3" (lambda (&rest _ignored) (pgmacstbl-goto-column 3))
                                  "4" (lambda (&rest _ignored) (pgmacstbl-goto-column 4))
                                  "5" (lambda (&rest _ignored) (pgmacstbl-goto-column 5))
                                  "6" (lambda (&rest _ignored) (pgmacstbl-goto-column 6))
                                  "7" (lambda (&rest _ignored) (pgmacstbl-goto-column 7))
                                  "8" (lambda (&rest _ignored) (pgmacstbl-goto-column 8))
                                  "9" (lambda (&rest _ignored) (pgmacstbl-goto-column 9))
                                  "T" pgmacs--switch-to-database-buffer
                                  "q" (lambda (&rest ignore) (bury-buffer))))))
      (setq-local pgmacs--con con
                  pgmacs--table table
                  pgmacs--offset offset
                  pgmacs--column-type-names (apply #'vector column-type-names)
                  buffer-read-only t
                  truncate-lines t)
      (pgmacs-mode)
      (use-local-map pgmacs-row-list-map)
      (when comment
        (insert (propertize "Table comment" 'face 'bold))
        (insert (format ": %s  " comment))
        (insert-text-button "Modify"
                            'action (lambda (&rest _ignore)
                                      (let ((comment (read-from-minibuffer "New table comment: ")))
                                        (setf (pg-table-comment con table) comment))
                                      (pgmacs--display-table table))
                            'help-echo "Modify the table comment")
        (insert "\n"))
      (let* ((sql "SELECT pg_catalog.pg_size_pretty(pg_catalog.pg_total_relation_size($1)),
                          pg_catalog.pg_size_pretty(pg_catalog.pg_indexes_size($1))")
             (res (pg-exec-prepared con sql `((,t-id . "text"))))
             (row (pg-result res  :tuple 0)))
        (insert (propertize "On-disk-size" 'face 'bold))
        (insert (format ": %s" (cl-first row)))
        (insert (format " (indexes %s)\n" (cl-second row))))
      (insert (propertize "Columns" 'face 'bold))
      (insert ":\n")
      (let ((colinfo (list)))
        (dolist (col column-names)
          (let* ((ci (gethash col column-info))
                 (cif (pgmacs--format-column-info ci)))
            (push (format "%s: %s" col cif) colinfo)))
        (let ((last (pop colinfo)))
          (dolist (c (reverse colinfo))
            (if (char-displayable-p ?├)
                (insert (propertize "├ " 'face 'shadow))
              (insert (propertize "| " 'face 'shadow)))
            (insert c)
            (insert "\n"))
          (if (char-displayable-p ?└)
              (insert (propertize "└ " 'face 'shadow))
            (insert (propertize "` " 'face 'shadow)))
          (insert last)
          (insert "\n")))
      (when indexes
        (insert (propertize "Indexes" 'face 'bold))
        (insert ":\n")
        (dolist (idx indexes)
          (cl-multiple-value-bind (name unique-p primary-p clustered-p valid-p _def type cols) idx
              (insert (format "  %s %s%s%s%s %s (cols: %s)\n"
                              name
                              (if unique-p "UNIQUE " "")
                              (if primary-p "PRIMARY " "")
                              (if clustered-p "CLUSTERED " "")
                              (if valid-p "" "INVALID ")
                              type cols)))))
      (insert "Row-level access control: ")
      (if (pgmacs--row-security-active con table)
          (insert "enabled")
        (insert "not enabled"))
      (insert "\n\n")
      (insert-text-button "Export table to CSV buffer"
                          'action #'pgmacs--table-to-csv
                          'help-echo "Export this table to a CSV buffer")
      (unless primary-keys
        (insert "   ")
        (insert-text-button "Add primary key to table"
                            'action #'pgmacs--add-primary-key
                            'help-echo "Add a PRIMARY KEY to enable editing"))
      (insert "  ")
      (insert-text-button "Count rows"
                          'action #'pgmacs--run-count
                          'help-echo "Count rows in this table")
      (insert "  ")
      (insert-text-button "ANALYZE this table"
                          'action #'pgmacs--run-analyze
                          'help-echo "Run ANALYZE on this table")
      (unless comment
        (insert "\n")
        (insert-text-button "Add table comment"
                            'action (lambda (&rest _ignore)
                                      (let ((comment (read-from-minibuffer "Table comment: ")))
                                        (setf (pg-table-comment con table) comment))
                                      (pgmacs--display-table table))
                            'help-echo "Add an SQL comment to the table"))
      (insert "\n\n")
      ;; Make it visually clear to the user that a WHERE filter is active
      (when where-filter
        (insert (propertize "WHERE filter" 'face 'bold))
        (insert ": ")
        (insert (propertize where-filter 'face 'pgmacs-where-filter))
        (insert "\n\n"))
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
      ;; Recreate the row markings from the line numbers stored in pgmacs--marked-rows, if necessary
      ;; (this function may be called to update a pgmacstbl after a modification, in which case
      ;; pgmacs--marked-rows may be non-nil).
      (let* ((buffer-read-only nil))
        (dolist (line-number pgmacs--marked-rows)
          (pgmacstbl-mark-row pgmacstbl line-number :marked-for-deletion)
          (save-excursion
            (let ((object (elt (pgmacstbl-objects pgmacstbl) line-number)))
              (pgmacstbl-goto-object object)
              (add-face-text-property (pos-bol) (pos-eol) '(:background "red"))))))
      (pgmacs--stop-progress-reporter)
      ;; if asked to center-on a particular pk value, search for it and move point to that row
      (when center-on
        (let* ((pk-name (cl-first center-on))
               (pk-val (cl-second center-on))
               (cols (pgmacstbl-columns pgmacstbl))
               (pk-col-id (cl-position pk-name cols :key #'pgmacstbl-column-name :test #'string=)))
          (unless pk-col-id
            (error "Can't find column named %s" pk-name))
          (cl-loop named position-cursor
           for row in (pgmacstbl-objects pgmacstbl)
           when (equal (nth pk-col-id row) pk-val) do
           (progn
             (pgmacstbl-goto-object row)
             (pgmacstbl-goto-column pk-col-id)
             (cl-return-from position-cursor))
           finally do (message "Didn't find row matching %s" pk-val)))))))

(defun pgmacs--row-list-redraw (&rest _ignore)
  "Refresh a PostgreSQL row-list buffer.
This refetches data from PostgreSQL."
  (interactive)
  (let ((table pgmacs--table)
        (offset pgmacs--offset)
        (where-filter pgmacs--where-filter)
        (parent-buffer pgmacs--db-buffer))
    (kill-buffer)
    ;; We need to reset all row marks, because the PostgreSQL table may have seen changes since our
    ;; previous fetch of rows, so the line numbers in the pgmacstbl may not longer be relevant.
    (setq pgmacs--marked-rows (list))
    ;; Make sure we switch back to the main PGmacs buffer before recreating the row-list buffer,
    ;; because this "parent" buffer holds buffer-local variables that we need to connect to
    ;; PostgreSQL.
    (with-current-buffer parent-buffer
      (pgmacs--display-table table :where-filter where-filter)
      (setq pgmacs--offset offset))))

;; Shrink the current column size to the smallest possible for the values that are currently visible.
(defun pgmacs--shrink-column (&rest _ignore)
  (let* ((pgmacstbl (pgmacstbl-current-table))
         (widths (pgmacstbl--widths pgmacstbl))
         (cols (pgmacstbl-columns pgmacstbl))
         (col-id (pgmacstbl-current-column))
         (col (elt cols col-id))
         (col-name (pgmacstbl-column-name col))
         (max-width (1+ (string-width col-name))))
    (dolist (row (pgmacstbl-objects pgmacstbl))
      (let* ((val (nth col-id row))
             ;; FIXME perhaps should call the column display-function here
             (valstr (if (stringp val) val (format "%s" val)))
             (valw (string-width valstr)))
        (setq max-width (max max-width valw))))
    (setf (aref widths col-id) (* (pgmacstbl--char-width pgmacstbl) max-width))
    (setf (pgmacstbl-column-width col) (format "%dpx" max-width)))
  (pgmacs--redraw-pgmacstbl))

;; Shrink each column to the smallest size possible.
(defun pgmacs--shrink-columns (&rest _ignore)
  (let* ((pgmacstbl (pgmacstbl-current-table))
         (widths (pgmacstbl--widths pgmacstbl))
         (cols (pgmacstbl-columns pgmacstbl)))
    (dotimes (col-id (length cols))
      (let* ((col (elt cols col-id))
             (col-name (pgmacstbl-column-name col))
             (max-width (1+ (string-width col-name))))
        (dolist (row (pgmacstbl-objects pgmacstbl))
          (let* ((val (nth col-id row))
                 ;; FIXME perhaps should call the column display-function here
                 (valstr (if (stringp val) val (format "%s" val)))
                 (valw (string-width valstr)))
            (setq max-width (max max-width valw))))
        (setf (aref widths col-id) (* (pgmacstbl--char-width pgmacstbl) max-width))))
    (pgmacs--redraw-pgmacstbl)))


;; Used in table-list buffer: if point is on a column which REFERENCES a foreign table, then jump to
;; that table on the appropriate row; otherwise prompt to edit using pgmacs--edit-value-minibuffer
(defun pgmacs--table-list-dwim (row primary-keys)
  (let* ((colinfo (get-text-property (point) 'pgmacs--column-info))
         (refs (and colinfo (gethash "REFERENCES" colinfo))))
    (if refs
        (let* ((table (cl-first refs))
               (pk (cl-second refs))
               (pk-col-id (pgmacstbl-current-column))
               (pk-col-type (aref pgmacs--column-type-names pk-col-id))
               (pk-val (nth pk-col-id row))
               (center-on (list pk pk-val pk-col-type)))
          (pgmacs--display-table table :center-on center-on))
      (pgmacs--edit-value-minibuffer row primary-keys))))

;; bound to "o". We make sure here to retain a schema-qualified name for a table, because
;; pgmacs--display-table needs a schema-qualified name for tables not in the current schema.
(defun pgmacs-open-table (&rest _ignore)
  "Open a row-list buffer for a PostgreSQL table.
Prompt for the table name in the minibuffer."
  (interactive)
  (let* ((tables (pg-tables pgmacs--con))
         (completions (mapcar (lambda (mqn) (cons (pgmacs--display-identifier mqn) mqn)) tables))
         (identifier (completing-read "PostgreSQL table: " completions nil t))
         (table (or (cdr (assoc identifier completions #'string=))
                    identifier)))
    (pgmacs--display-table table)))

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
  (let ((con pgmacs--con)
        (db-buffer pgmacs--db-buffer))
    (pop-to-buffer (get-buffer-create "*PostgreSQL backend information*"))
    (erase-buffer)
    (remove-overlays)
    (kill-all-local-variables)
    (buffer-disable-undo)
    (setq-local pgmacs--db-buffer db-buffer)
    (let* ((res (pg-exec con "SELECT inet_server_addr(), inet_server_port(), pg_backend_pid()"))
           (row (pg-result res :tuple 0))
           (addr (cl-first row))
           (port (cl-second row))
           (pid (cl-third row)))
      (insert (format "Connected to backend with pid %s" pid))
      (if addr
          (insert (format " at %s:%s\n" addr port))
        (insert " over Unix-domain socket\n")))
    (let* ((res (pg-exec con "SELECT pg_catalog.current_setting('ssl_library')"))
           (row (pg-result res :tuple 0)))
      (insert (format "Backend compiled with SSL library %s\n" (cl-first row))))
    (let* ((res (pg-exec con "SELECT current_user, pg_catalog.current_setting('is_superuser')"))
           (row (pg-result res :tuple 0)))
      (insert (format "Connected as user %s (%ssuperuser)\n"
                      (cl-first row)
                      (if (cl-second row) "" "not "))))
    (let* ((res (pg-exec con "SELECT pg_catalog.current_setting('in_hot_standby')"))
           (row (pg-result res :tuple 0)))
      (insert (apply #'format "In hot standby: %s\n" row)))
    (let* ((res (pg-exec con "SELECT pg_catalog.pg_postmaster_start_time()"))
           (dtime (car (pg-result res :tuple 0)))
           (fmt (funcall (pgmacs--value-formatter "timestamp") dtime)))
      (insert (format "PostgreSQL running since %s\n" fmt)))
    (let* ((res (pg-exec con "SELECT pg_catalog.current_setting('client_encoding')"))
           (row (pg-result res :tuple 0)))
      (insert (apply #'format "Client encoding: %s\n" row)))
    (let* ((res (pg-exec con "SELECT pg_catalog.current_setting('server_encoding')"))
           (row (pg-result res :tuple 0)))
      (insert (apply #'format "Server encoding: %s\n" row)))
    (let* ((res (pg-exec con "SELECT pg_catalog.current_setting('TimeZone')"))
           (row (pg-result res :tuple 0)))
      (insert (apply #'format "Server timezone: %s\n" row)))
    (let* ((res (pg-exec con "SELECT pg_catalog.current_setting('shared_memory_size')"))
           (row (pg-result res :tuple 0)))
      (insert (format "Server shared memory size: %s\n" (cl-first row))))
    (let* ((res (pg-exec con "SELECT pg_catalog.pg_listening_channels()"))
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
      (dolist (ext exts)
        (insert (apply #'format "%30s %17s %18s" ext))
        (unless (or (cl-third ext)
                    (string= "Name" (cl-first ext)))
          (insert "  ")
          (insert-text-button
           "Load extension"
           'action (lambda (&rest _ignore)
                     (message "Loading PostgreSQL extension %s" (car ext))
                     (condition-case nil
                         (pg-exec con (format "CREATE EXTENSION IF NOT EXISTS %s" (car ext)))
                       (pg-error
                        (lambda (err)
                          (message "Loading extension %s failed: %s" (car ext) err)))))))
        (insert "\n")))
    (shrink-window-if-larger-than-buffer)
    (goto-char (point-min))
    (help-mode)
    (pgmacs-transient-mode)))


(defvar pgmacs--stat-activity-columns
  (list "datname" "usename" "client_addr" "backend_start" "xact_start" "query_start" "wait_event"))

(defun pgmacs--display-stat-activity (&rest _ignore)
  "Display information from PostgreSQL's pg_stat_activity table."
  (let* ((cols (string-join pgmacs--stat-activity-columns ","))
         (sql (format "SELECT %s FROM pg_catalog.pg_stat_activity" cols)))
    (pgmacs-show-result pgmacs--con sql)))

(defvar pgmacs--run-sql-history nil)

(defun pgmacs-run-sql (&rest _ignore)
  "Prompt for an SQL query and display the output in a dedicated buffer."
  (interactive)
  (let ((sql (read-from-minibuffer "SQL query: " nil nil nil 'pgmacs--run-sql-history)))
    (pgmacs-show-result pgmacs--con sql)))

(defun pgmacs-run-buffer-sql (&rest _ignore)
  "Execute the SQL query in a user-specified buffer.
The output is displayed in a dedicated buffer."
  (interactive)
  (let* ((con pgmacs--con)
         (buffers (mapcar #'buffer-name (buffer-list)))
         (buffer (completing-read "Run SQL from buffer: " buffers nil t)))
    (with-current-buffer buffer
      (let ((sql (buffer-substring-no-properties (point-min) (point-max))))
        (pgmacs-show-result con sql)))))

;; TODO: should limit this function to returning the pgmacstbl and let the caller pop to buffer, set
;; pgmacs-mode, insert the information text.
(defun pgmacs-show-result (con sql)
  "Create a buffer to show the results of PostgreSQL query SQL.
Uses PostgreSQL connection CON."
  (let ((db-buffer pgmacs--db-buffer))
    (pop-to-buffer (get-buffer-create "*PostgreSQL TMP*"))
    (erase-buffer)
    (remove-overlays)
    (kill-all-local-variables)
    (buffer-disable-undo)
    (setq-local pgmacs--con con
                pgmacs--db-buffer db-buffer
                buffer-read-only t
                truncate-lines t))
  (pgmacs-mode)
  (pgmacs--start-progress-reporter "Retrieving data from PostgreSQL")
  ;; Insert initial content into buffer early.
  (let ((inhibit-read-only t))
    (insert (propertize "PostgreSQL query output" 'face 'bold))
    (insert "\n")
    (insert (propertize "SQL" 'face 'bold))
    (insert (format ": %s\n\n" sql)))
  (let* ((res (pg-exec con sql)))
    (pgmacs--show-pgresult (current-buffer) res)))

(defun pgmacs--show-pgresult (buffer pgresult)
  (with-current-buffer buffer
    (let ((rows (pg-result pgresult :tuples))
          (attributes (pg-result pgresult :attributes))
          (con pgmacs--con))
      (cond ((null rows)
             (insert "(no rows)"))
            (t
             (let* ((column-names (mapcar #'cl-first attributes))
                    (column-type-oids (mapcar #'cl-second attributes))
                    (column-type-names (mapcar (lambda (o) (pg-lookup-type-name con o)) column-type-oids))
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
                                               :formatter fmt
                                               :displayer (pgmacs--make-column-displayer "" nil))))
                    (inhibit-read-only t)
                    (pgmacstbl (make-pgmacstbl
                                :insert nil
                                :use-header-line nil
                                :face 'pgmacs-table-data
                                :columns columns
                                :row-colors pgmacs-row-colors
                                :objects rows
                                :actions '("e" pgmacs-run-sql
                                           "E" pgmacs-run-buffer-sql
                                           "r" pgmacs--redraw-pgmacstbl
                                           "j" pgmacs--row-as-json
                                           "v" pgmacs--view-value
                                           "o" pgmacs-open-table
                                           ;; "n" and "p" are bound when table is paginated to next/prev page
                                           "=" pgmacs--shrink-columns
                                           "<" (lambda (&rest _ignored)
                                                 (text-property-search-backward 'pgmacstbl)
                                                 (next-line))
                                           ">" (lambda (&rest _ignored)
                                                 (text-property-search-forward 'pgmacstbl)
                                                 (previous-line))
                                           "0" (lambda (&rest _ignored) (pgmacstbl-goto-column 0))
                                           "1" (lambda (&rest _ignored) (pgmacstbl-goto-column 1))
                                           "2" (lambda (&rest _ignored) (pgmacstbl-goto-column 2))
                                           "3" (lambda (&rest _ignored) (pgmacstbl-goto-column 3))
                                           "4" (lambda (&rest _ignored) (pgmacstbl-goto-column 4))
                                           "5" (lambda (&rest _ignored) (pgmacstbl-goto-column 5))
                                           "6" (lambda (&rest _ignored) (pgmacstbl-goto-column 6))
                                           "7" (lambda (&rest _ignored) (pgmacstbl-goto-column 7))
                                           "8" (lambda (&rest _ignored) (pgmacstbl-goto-column 8))
                                           "9" (lambda (&rest _ignored) (pgmacstbl-goto-column 9))
                                           "q" (lambda (&rest _ignore) (bury-buffer))))))
               (pgmacstbl-insert pgmacstbl))))
      (shrink-window-if-larger-than-buffer)
      (pgmacs--stop-progress-reporter))))


;; If the cursor is on the Comment column, allow the user to set the table comment. Otherwise,
;; display the table in a separate buffer.
(defun pgmacs--table-list-RET (table-row)
  "Called on RET on a line in the list-of-tables buffer TABLE-ROW."
  (let* ((tbl (pgmacstbl-current-table))
         (col-id (pgmacstbl-current-column))
         (col (nth col-id (pgmacstbl-columns tbl)))
         (col-name (pgmacstbl-column-name col))
         (table (cl-first table-row)))
    (cond ((string= "Comment" col-name)
           (let ((comment (read-from-minibuffer "New table comment: "))
                 (new-row (copy-sequence table-row)))
             (setf (pg-table-comment pgmacs--con table) comment)
             (setf (nth col-id new-row) comment)
             ;; pgmacstbl-update-object doesn't work, so insert then delete old row
             (pgmacstbl-insert-object tbl new-row table-row)
             (pgmacstbl-remove-object tbl table-row)
             (pgmacs--redraw-pgmacstbl)))
          ;; TODO perhaps allow changes to table owner (if we are superuser)
          (t
           (pgmacs--display-table table)))))

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
  (buffer-disable-undo)
  (help-mode)
  (cl-flet ((shw (key msg)
              (insert (propertize (format "%12s" key) 'face '(:foreground "blue")))
              (insert (propertize " → " 'face '(:foreground "gray")))
              (insert msg "\n")))
    (let ((inhibit-read-only t))
      (shw "RET" "Open a new buffer to browse/edit the table at point")
      (shw "<deletechar>" "Delete the table at point")
      (shw "r" "Rename the table at point")
      (shw "o" "Prompt for a table to browse/edit in a new buffer")
      (shw "p" "New buffer listing the functions and procedures in the current database")
      (shw "e" "New buffer with output from SQL query")
      (shw "E" "Run buffer SQL and display the output")
      (shw "S" "Run SchemaSpy on the current database and display the SVG output")
      (shw "<" "Go to the first table in the table list")
      (shw ">" "Go to the last table in the table list")
      (shw "{" "Shrink the horizontal space used by the current column")
      (shw "}" "Grow the horizontal space used by the current column")
      (shw "g" "Redraw this table-list buffer (refetches data from PostgreSQL)")
      (shw "q" "Bury this buffer")
      (shrink-window-if-larger-than-buffer)
      (goto-char (point-min)))))

(defun pgmacs--rewrite-schemaspy-svg (svg-pathname)
  "Clean up the SVG produced by SchemaSpy to improve display in Emacs.
The SVG produced by dot includes xlinked references to PNG files
that represent a key. These xlinked relative pathnames are not
displayed by the SVG support in Emacs. We replace them by an
inlined vector SVG image that is encoded as a data URI."
  (with-current-buffer (find-file-noselect svg-pathname t t)
    (cl-loop while (search-forward "xlink:href=\"../../images/primaryKeys.png\"" nil t)
             do (delete-region (match-beginning 0) (match-end 0))
             (insert "href=\"data:image/svg+xml,%3Csvg height='1362.976' viewBox='0 0 154.491 360.621' width='583.904' xmlns='http://www.w3.org/2000/svg'%3E%3Cpath d='M69.407 360.143c-8.545-1.912-12.989-5.943-15.73-14.271-1.263-3.84-1.716-7.444-3.181-25.306-2.999-36.562-4.773-64.722-6.027-95.69-.332-8.209-5.756-80.762-6.048-80.913-1.011-.52-8.86-5.98-10.696-7.44-3.498-2.783-9.747-9.042-12.344-12.364-6.935-8.871-11.455-18.535-13.708-29.314C.345 88.495-.015 84.632 0 76.886.026 63.563 2.333 53.394 7.8 42.49c3.93-7.834 8.006-13.364 14.96-20.295C35.21 9.785 49.463 2.732 66.707.446c5.045-.669 17.296-.567 22.225.184 11.299 1.723 21.418 5.533 30.339 11.423 8.456 5.583 16.442 13.447 22.31 21.97 11.561 16.793 15.58 38.777 11.155 61.031-3.871 19.471-14.575 35.045-32.054 46.637-2.62 1.738-4.966 3.545-5.216 4.016-1.007 1.905-3.055 7.729-4.826 13.73-1.734 5.874-4.422 16.976-4.422 18.264 0 .922-1.641 2.581-4.62 4.668-4.347 3.046-4.196 2.561-4.198 13.51-.001 10.612-.118 10.114 3.192 13.672 1.14 1.226 2.178 2.644 2.305 3.151.128.508.164 4.288.081 8.4-.122 6.066-.26 7.679-.73 8.537a35.426 35.426 0 0 0-.942 1.852c-.2.437-.534.794-.745.794-.646 0-4.22 3.592-4.22 4.241 0 .336-.25.818-.553 1.07-1.505 1.25-2.785 6.552-2.546 10.552.213 3.566.906 4.789 5.328 9.41 5.078 5.308 5.002 5.13 5.002 11.769 0 6.762-.117 7.03-5.466 12.523-2.079 2.134-4.041 4.389-4.36 5.01-.5.971-.58 2.548-.579 11.289.001 5.913.155 10.709.367 11.474.201.723.915 2.034 1.586 2.914.671.879 1.22 1.8 1.22 2.045s1.192 1.63 2.648 3.077c3.952 3.926 3.887 3.641 3.837 16.755-.047 12.092-.36 14.766-2.187 18.67-1.867 3.99-3.773 5.27-9.766 6.559-4.77 1.026-17.76 1.329-21.465.5z' fill='%23d6de00'/%3E%3Cpath d='M96.814 73.36a25.972 26.755 0 0 1-36.667-1.957 25.972 26.755 0 0 1 1.871-37.773 25.972 26.755 0 0 1 36.67 1.898 25.972 26.755 0 0 1-1.815 37.776' fill='%23fff'/%3E%3C/svg%3E\""))
    ;; drop the "Generated by SchemaSpy label" which is rather intrusive
    (goto-char (point-min))
    (when (search-forward "Generated by SchemaSpy</text>" nil t)
      (delete-region (line-beginning-position) (line-end-position)))
    (save-buffer)
    (kill-buffer)))

(defun pgmacs--schemaspy-table (&rest _ignore)
  "Run SchemaSpy on current table and display the SVG describing the schema."
  (interactive)
  (unless (display-graphic-p)
    (user-error "SchemaSpy will only work on a graphical terminal"))
  (unless (image-type-available-p 'svg)
    (user-error "SchemaSpy support needs SVG support in your Emacs"))
  (let* ((tmpdir (temporary-file-directory))
         (schemaspy-dir (expand-file-name "pgmacs-schemaspy" tmpdir)))
    (when (file-directory-p schemaspy-dir)
      (delete-directory schemaspy-dir t nil))
    (with-file-modes #o777
      (make-directory schemaspy-dir t))
    ;; The Docker image for schemaspy runs as user "java" for an obscure reason, so ensure that the
    ;; temporary schemaspy-dir is writable for all.
    (let ((ci (pgcon-connect-info pgmacs--con))
          (schema-name (if (pg-qualified-name-p pgmacs--table)
                           (pg-qualified-name-schema pgmacs--table)
                         "public"))
          (table-name (if (pg-qualified-name-p pgmacs--table)
                          (pg-qualified-name-name pgmacs--table)
                        pgmacs--table)))
      (when (eql :local (cl-first ci))
        (message "Replacing Unix connection by network connection to localhost for SchemaSpy"))
      (let* ((cmdline (concat pgmacs-schemaspy-cmdline
                              " -s %s -i %t"))
             (cmd (cl-multiple-value-bind (type host port dbname user password) ci
                    (let ((spec (list (cons ?h (if (eq type :local) "localhost" host))
                                      (cons ?P (or port 5432))
                                      (cons ?d dbname)
                                      (cons ?u user)
                                      (cons ?p password)
                                      (cons ?s schema-name)
                                      (cons ?t table-name)
                                      (cons ?D schemaspy-dir))))
                      (format-spec cmdline spec))))
             (out (format "%s/diagrams/tables/%s.1degree.svg"
                          schemaspy-dir
                          table-name)))
        (message "Running cmd %s, output to %s" cmd out)
        (shell-command cmd)
        (when (file-exists-p out)
          (pgmacs--rewrite-schemaspy-svg out)
          (find-file out))))))

;; Run SchemaSpy on the current database, display the SVG. We display only the "real relationships"
;; summary SVG for the database; SchemaSpy generates many other images including for each orphan
;; table.
(defun pgmacs--schemaspy-database (&rest _ignore)
  (interactive)
  (unless (display-graphic-p)
    (user-error "SchemaSpy will only work on a graphical terminal"))
  (unless (image-type-available-p 'svg)
    (user-error "SchemaSpy support needs SVG support in your Emacs"))
  (let* ((tmpdir (temporary-file-directory))
         (schemaspy-dir (expand-file-name "pgmacs-schemaspy" tmpdir)))
    (when (file-directory-p schemaspy-dir)
      (delete-directory schemaspy-dir t))
    ;; The Docker image for schemaspy runs as user "java" for an obscure reason, so we need to
    ;; ensure that the temporary schemaspy-dir is writable for all.
    (with-file-modes #o777
      (make-directory schemaspy-dir t))
    (let ((ci (pgcon-connect-info pgmacs--con)))
      (when (eql :local (cl-first ci))
        (message "Replacing Unix connection by network connection to localhost for SchemaSpy"))
      (let ((cmd (cl-multiple-value-bind (type host port dbname user password) ci
                   (let ((spec (list (cons ?h (if (eq type :local) "localhost" host))
                                     (cons ?P (or port 5432))
                                     (cons ?d dbname)
                                     (cons ?u user)
                                     (cons ?p password)
                                     (cons ?D schemaspy-dir))))
                     (format-spec pgmacs-schemaspy-cmdline spec))))
            (out (format "%s/diagrams/summary/relationships.real.compact.svg" schemaspy-dir)))
        (message "Running cmd %s, output to %s" cmd out)
        (shell-command cmd)
        (when (file-exists-p out)
          (pgmacs--rewrite-schemaspy-svg out)
          (find-file out))))))

(defun pgmacs--svg-icon-database ()
  (let ((icon (svg-create "1.3em" "1.3em" :viewBox "0 0 16 16")))
    (cl-flet ((ellipse (y)
                (svg-ellipse icon 8 y 5 1 :fill "purple")
                (svg-ellipse icon 8 (+ y 1.8) 5 1 :fill "purple" :stroke "white" :stroke-width 0.3)
                (svg-rectangle icon 3 y 10 1.5 :fill "purple")))
      (ellipse 9)
      (ellipse 6)
      (ellipse 3)
      (svg-image icon :margin 2 :ascent 'center))))

(defun pgmacs--svg-icon-user ()
  (let ((icon (svg-create "0.7em" "0.7em" :viewBox "0 0 16 16")))
    (svg-circle icon 8 4 3.8 :fill "black" :opacity 0.5)
    (svg-rectangle icon 1 9 15 20 :fill "black" :opacity 0.5 :rx 3)
    (svg-image icon :margin 2 :ascent 'center)))

(defun pgmacs--svg-icon-table ()
  (let ((icon (svg-create "1em" "1em" :viewBox "-0.4 -0.4 3.6 4.6" :fill "currentColor")))
    (svg-rectangle icon 0 0 3 3 :fill "none" :stroke "black" :stroke-width 0.3 :rx 0.1)
    (svg-line icon 0 1 3 1 :stroke "black" :stroke-width 0.2)
    (svg-line icon 0 2 3 2 :stroke "black" :stroke-width 0.2)
    (svg-line icon 0 0.3 3 0.3 :stroke "black" :stroke-width 0.2)
    (svg-line icon 1 0 1 3 :stroke "black" :stroke-width 0.2)
    (svg-line icon 2 0 2 3 :stroke "black" :stroke-width 0.2)
    (svg-image icon :margin 2 :ascent 'center)))

;; This function generates the string used to display a table in the main table-list buffer. If the
;; display is able to display SVG images, we prefix the name with a little SVG icon of a table.
(defun pgmacs--display-table-name (name)
  (let* ((name (pgmacs--display-identifier name))
         (maybe-icon (pgmacs--maybe-svg-icon #'pgmacs--svg-icon-table)))
    (concat maybe-icon name)))


;;;###autoload
(defun pgmacs-open (con)
  "Browse the contents of PostgreSQL database to which we are connected over CON."
  (when pgmacs-enable-query-logging
    (message "Enabling PGmacs query logging")
    (pg-enable-query-log con))
  (pg-hstore-setup con)
  (pg-vector-setup con)
  (pop-to-buffer-same-window (format "*PostgreSQL %s*" (pgcon-dbname con)))
  (setq-local pgmacs--con con
              pgmacs--db-buffer (current-buffer)
              buffer-read-only t
              truncate-lines t)
  (pgmacs-mode)
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
                             :align 'left)
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
                             "?" pgmacs--table-list-help
                             "RET" pgmacs--table-list-RET
                             "<deletechar>" pgmacs--table-list-delete
                             "S" pgmacs--schemaspy-database
                             "r" pgmacs--table-list-rename
                             "g" pgmacs--table-list-redraw
                             "o" pgmacs-open-table
                             "e" pgmacs-run-sql
                             "E" pgmacs-run-buffer-sql
                             ;; the functions pgmacstbl-beginning-of-table and
                             ;; pgmacstbl-end-of-table don't work when we have inserted text before
                             ;; the pgmacstbl.
                             "<" (lambda (&rest _ignored)
                                   (text-property-search-backward 'pgmacstbl)
                                   (next-line))
                             ">" (lambda (&rest _ignored)
                                   (text-property-search-forward 'pgmacstbl)
                                   (previous-line))
                             "q"  (lambda (&rest _ignored) (bury-buffer)))
                  :getter (lambda (object column pgmacstbl)
                            (pcase (pgmacstbl-column pgmacstbl column)
                              ("Table" (pgmacs--display-table-name (cl-first object)))
                              ("Rows" (cl-second object))
                              ("Size on disk" (cl-third object))
                              ("Owner" (cl-fourth object))
                              ("Comment" (cl-fifth object)))))))
    (let* ((res (pg-exec con "SELECT current_user, pg_backend_pid(), pg_is_in_recovery()"))
           (row (pg-result res :tuple 0)))
      (insert (format "\nConnected to database %s%s as %s%s (pid %d %s)\n"
                      (pgmacs--maybe-svg-icon #'pgmacs--svg-icon-database)
                      dbname
                      (pgmacs--maybe-svg-icon #'pgmacs--svg-icon-user)
                      (cl-first row)
                      (cl-second row)
                      (if (cl-third row) "RECOVERING" "PRIMARY"))))
    (let* ((sql "SELECT pg_catalog.pg_size_pretty(pg_catalog.pg_database_size($1))")
           (res (pg-exec-prepared con sql `((,dbname . "text"))))
           (size (cl-first (pg-result res :tuple 0))))
      (insert (format "Total database size: %s\n" size)))
    ;; Perhaps also display output from
    ;; select state, count(*) from pg_stat_activity where pid <> pg_backend_pid() group by 1 order by 1;'
    ;; see https://gitlab.com/postgres-ai/postgresql-consulting/postgres-howtos/-/blob/main/0068_psql_shortcuts.md
    (insert "\n")
    (insert-text-button "Display procedures"
                        'action #'pgmacs--display-procedures)
    (insert "   ")
    (insert-text-button "Display running queries"
                        'action #'pgmacs--display-running-queries)
    (insert "   ")
    (insert-text-button "More backend information"
                        'action #'pgmacs--display-backend-information)
    (insert "   \n")
    (insert-text-button "PostgreSQL settings"
                        'action (lambda (&rest _ignore)
                                  (pgmacs-show-result con "SELECT * FROM pg_settings")))
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


(defvar pgmacs--open-history (list))

;;;###autoload
(defun pgmacs-open-string (connection-string)
  "Open PGmacs on database specified by CONNECTION-STRING.
CONNECTION-STRING is a PostgreSQL connection string of the form
`dbname=mydb user=me host=localhost password=foo'.
The supported keywords in the connection string are host,
hostaddr, port, dbname, user, password, sslmode (partial support)
and application_name."
  (interactive
   (list (read-string "PostgreSQL connection string: " nil 'pgmacs--open-history)))
  (pgmacs--start-progress-reporter "Connecting to PostgreSQL")
  (pgmacs-open (pg-connect/string connection-string)))

;;;###autoload
(defun pgmacs-open-uri (connection-uri)
  "Open PGmacs on database specified by CONNECTION-URI.
CONNECTION-URI is a PostgreSQL connection URI of the form
`postgresql://user:pass@host/dbname'."
  (interactive
   (list (read-string "PostgreSQL connection URI: " nil 'pgmacs--open-history "postgresql://")))
  (pgmacs--start-progress-reporter "Connecting to PostgreSQL")
  (pgmacs-open (pg-connect/uri connection-uri)))

;; The environment variables that we look for to pre-populate the login widget are
;; semi-standardized, used for example in the official Docker image for PostgreSQL.
;;
;;    https://hub.docker.com/_/postgres/
;;
;; and the more sophisticated Bitnami PostgreSQL image
;;
;;    https://registry.hub.docker.com/r/bitnami/postgresql#!
;;
;; and from libpq/psql.
;;;###autoload
(defun pgmacs ()
  "Open a widget-based login buffer for PostgreSQL.
Widget fields are pre-populated by the values of the following
enviroment variables, if set:

- POSTGRES_DATABASE, POSTGRESQL_DATABASE, POSTGRES_DB, PGDATABASE
- POSTGRES_HOSTNAME, PGHOST
- POSTGRES_PORT_NUMBER, POSTGRESQL_PORT_NUMBER, PGPORT
- POSTGRES_USER, POSTGRESQL_USERNAME, PGUSER
- POSTGRES_PASSWORD, POSTGRESQL_PASSWORD, PGPASSWORD
"
  (interactive)
  (switch-to-buffer "*PGmacs connection widget*")
  (kill-all-local-variables)
  (remove-overlays)
  (pgmacs--widget-setup)
  (widget-insert (propertize "Connect to PostgreSQL database" 'face 'bold))
  (widget-insert "\n\n")
  (let* ((w-dbname
          (progn
            (widget-insert (format "%18s: " "Database name"))
            (widget-create 'editable-field
                           :size 20
                           (or (getenv "POSTGRES_DATABASE")
                               (getenv "POSTGRESQL_DATABASE")
                               (getenv "POSTGRES_DB")
                               ;; this one accepted by libpq
                               (getenv "PGDATABASE")
                               ""))))
         (w-hostname
          (progn
            (widget-insert (format "\n%18s: " "Hostname"))
            (widget-create 'editable-field
                           :help-echo "The host where PostgreSQL is running"
                           :default ""
                           :size 20
                           (or (getenv "POSTGRES_HOSTNAME")
                               ;; accepted by libpq
                               (getenv "PGHOST")
                               "localhost"))))
        (w-port
         (progn
           (widget-insert (format "\n%18s: " "Port"))
           (widget-create 'natnum
                          :format "%v"
                          :size 20
                          (or (getenv "POSTGRES_PORT_NUMBER")
                              (getenv "POSTGRESQL_PORT_NUMBER")
                              (getenv "PGPORT")
                              5432))))
        (w-username
         (progn
           (widget-insert (format "\n%18s: " "Username"))
           (widget-create 'editable-field
                          :help-echo "Authenticate as this user"
                          :size 20
                          (or (getenv "POSTGRES_USER")
                              (getenv "POSTGRESQL_USERNAME")
                              ;; this one accepted by libpq
                              (getenv "PGUSER")
                              ""))))
        (w-password
         (progn
           (widget-insert (format "\n%18s: " "Password"))
           (widget-create 'editable-field
                          :secret ?*
                          :size 20
                          (or (getenv "POSTGRES_PASSWORD")
                              (getenv "POSTGRESQL_PASSWORD")
                              ;; this one accepted by libpq
                              (getenv "PGPASSWORD")
                              ""))))
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


(pgmacstbl-register-mark-face :marked-for-deletion '(:background "red"))


(provide 'pgmacs)

;;; pgmacs.el ends here
