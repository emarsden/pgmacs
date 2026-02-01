;;; pgmacs.el --- Emacs is editing a PostgreSQL database  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Eric Marsden
;; Author: Eric Marsden <eric.marsden@risk-engineering.org>
;; Version: 0.29
;; Package-Requires: ((emacs "29.1") (pg "0.62"))
;; URL: https://github.com/emarsden/pgmacs/
;; Keywords: data, PostgreSQL, database
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; See README.md at https://github.com/emarsden/pgmacs/


;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'button)
(require 'widget)
(require 'wid-edit)
(require 'cus-edit)
(require 'svg)
(require 'rx)
(require 'sql)
(require 'pg)
(require 'pgmacstbl)
(require 'pgmacs-sql-keywords)
(require 'pgmacs-schemaspy)
(require 'pgmacs-chrome)


(declare-function csv-mode "csv-mode" ())
(declare-function markdown-mode "markdown-mode" ())
(declare-function markdown-cycle "markdown-mode" (&optional arg))



(defgroup pgmacs nil
  "Browse and edit a PostgreSQL database from Emacs."
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

(defface pgmacs-highlighted
  '((((class color) (background light))
     :weight bold
     :foreground "blue")
    (((class color) (background dark))
     :weight bold
     :foreground "lightblue"))
  "Face used to draw attention to text."
  :group 'pgmacs)

(defface pgmacs-muted
  '((((class color) (background light))
     :foreground "gray")
    (((class color) (background dark))
     :foreground "grey30"))
  "Face used to display muted text."
  :group 'pgmacs)

(defcustom pgmacs-deleted-color
  "red"
  "The background color used for table rows pending deletion."
  :type 'color
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

(defcustom pgmacs-large-database-threshold 100000000
  "Avoid index/table scans for databases above this size in octets.

For a database larger than this value (queried via PostgreSQL
function pg_database_size), PGmacs will estimate table row counts
using an imprecise method that does not require a full index (or
table) scan, but will provide invalid results for tables that
have not been VACUUMed or ANALYZEd. For sizes below this
threshold, a more accurate SELECT COUNT(*) FROM table_name query
will be used.

If set to zero, full index/table scans will never be issued (this
may be a safe option on large production databases)."
  :type 'number
  :group 'pgmacs)

(defcustom pgmacs-max-column-width 60
  "The maximal width in characters of a table column."
  :type 'number
  :group 'pgmacs)

(defcustom pgmacs-row-list-table-name-width 20
  "The number of characters used to display table names in row-list buffers."
  :type 'number
  :group 'pgmacs)

(defcustom pgmacs-row-list-comment-width 30
  "The number of characters used to display table comments in row-list buffers."
  :type 'number
  :group 'pgmacs)

(defcustom pgmacs-enable-query-logging nil
  "Whether SQL queries sent to PostgreSQL should be logged."
  :type 'boolean
  :group 'pgmacs)

(defcustom pgmacs-use-header-line t
  "If non-nil, use header line to display information on PostgreSQL connection."
  :type 'boolean
  :group 'pgmacs)

;; This is set to a value which displays information on the current PostgreSQL connection in
;; function pgmacs-open.
(defvar pgmacs-header-line nil
  "Header-line to use in PGmacs buffers. Nil to disable.")

(defcustom pgmacs-mode-hook nil
  "Mode hook for `pgmacs-mode'."
  :type 'hook
  :group 'pgmacs)

(defcustom pgmacs-table-list-hook nil
  "Functions to run after opening or refreshing the PGmacs table-list buffer."
  :type 'hook
  :group 'pgmacs)

(defcustom pgmacs-row-list-hook nil
  "Functions to run after opening or refreshing a PGmacs row-list buffer."
  :type 'hook
  :group 'pgmacs)

(defcustom pgmacs-timestamp-format "%Y-%m-%dT%T"
  "Format string to be used for timestamp/timestamptz/datetime output."
  :type 'string
  :group 'pgmacs)

(defcustom pgmacs-timestamp-zone nil
  "Time zone to be used for timestamp/timestamptz/datetime output,
e.g. `UTC' or `Europe/Berlin'. Nil for local OS timezone."
  :type 'string
  :group 'pgmacs)

;; TODO: it would be cleaner to hold these all in a pgmacs-connection object.
(defvar-local pgmacs--con nil)
(defvar-local pgmacs--table nil)
(defvar-local pgmacs--column-type-names nil)
(defvar-local pgmacs--offset nil)
(defvar-local pgmacs--db-buffer nil)
(defvar-local pgmacs--where-filter nil)
(defvar-local pgmacs--marked-rows (list))
(defvar-local pgmacs--completions nil)
(defvar-local pgmacs--table-primary-keys nil)




(defclass pgmacs-shortcut-button ()
  ((label :initarg :label :type string)
   ;; Condition, if defined, is a function called with zero arguments that determines whether this
   ;; button should be inserted, bassed on characteristics of the current table, for example. It can
   ;; use buffer-local variables pgmacs--con and pgmacs--table.
   (condition :initarg :condition :initform nil)
   (action :initarg :action)
   (help-echo :initarg :help-echo :initform nil)))

;; Insert OBJECT into the current buffer. Returns non-nil if the content was inserted (if its
;; condition slot evaluated to non-nil).
(cl-defgeneric pgmacs--insert (object))

(cl-defmethod pgmacs--insert ((btn pgmacs-shortcut-button))
  (with-slots (label condition action help-echo) btn
    (let ((disabled nil))
      (when condition
        (unless (funcall condition)
          (setq disabled t)))
      (unless disabled
        ;; TODO: the default keymap for these buttons binds TAB to next-button when point is inside
        ;; the button, but we would prefer to bind it to pgmacs--next-item (which moves to the data
        ;; table when it's on the last button, rather than wrapping to the first button in the
        ;; buffer). Perhaps we should override the 'keymap property on the button.
        ;; (svg-lib-button-mode)
        ;; (insert (svg-lib-button label action help-echo))))))
        (insert-text-button label 'action action 'help-echo help-echo)))))

;; It is possible to customize the row of text buttons that is displayed above the list of tables in
;; the main PGmacs buffer. Add an object that implements pgmacs--insert (such as a
;; pgmacs-shortcut-button object) to the pgmacs-table-list-buttons list.
(defvar pgmacs-table-list-buttons
  (list (pgmacs-shortcut-button
         :condition (lambda () (not (member (pgcon-server-variant pgmacs--con) '(questdb spanner materialize ydb))))
	 :label "Display procedures"
         :action #'pgmacs--display-procedures
         :help-echo "Display a table with all functions and procedures")
	(pgmacs-shortcut-button
         :condition (lambda () (not (member (pgcon-server-variant pgmacs--con) '(cratedb questdb spanner materialize risingwave))))
	 :label "Display running queries"
         :action #'pgmacs--display-running-queries)
        (pgmacs-shortcut-button
         :label "Display databases"
         :action #'pgmacs--display-database-list)
	(pgmacs-shortcut-button
         :condition (lambda () (not (member (pgcon-server-variant pgmacs--con) '(spanner))))
	 :label "More backend information"
         :action #'pgmacs--display-backend-information)
	(pgmacs-shortcut-button
         :condition (lambda () (not (member (pgcon-server-variant pgmacs--con) '(questdb ydb))))
	 :label (concat (if (char-displayable-p #x2699) "⚙️ " "")  "PostgreSQL settings")
         :action (lambda (&rest _ignore)
                   (pgmacs-show-result pgmacs--con "SELECT * FROM pg_settings")))
	(pgmacs-shortcut-button
         :condition (lambda () (not (member (pgcon-server-variant pgmacs--con) '(cratedb questdb spanner materialize risingwave))))
	 :label "Stat activity"
         :action #'pgmacs--display-stat-activity
         :help-echo "Show information from the pg_stat_activity table")
        (pgmacs-shortcut-button
         :condition (lambda () (eq (pgcon-server-variant pgmacs--con) 'risingwave))
         :label "Show jobs"
         :action #'pgmacs--display-jobs/risingwave
         :help-echo "Show all streaming jobs in process")
        (pgmacs-shortcut-button
         :condition (lambda () (eq (pgcon-server-variant pgmacs--con) 'materialize))
         :label "Show connections"
         :action #'pgmacs--display-connections/materialize
         :help-echo "Show all connections configured in Materialize")
	(pgmacs-shortcut-button
         :condition (lambda () (not (member (pgcon-server-variant pgmacs--con) '(cratedb questdb spanner materialize risingwave))))
	 :label (format "%sReplication stats" (or (pgmacs--maybe-svg-icon #'pgmacs--svg-icon-replication) ""))
	 :action #'pgmacs--display-replication-stats
	 :help-echo "Show information on PostgreSQL replication status")
        (pgmacs-shortcut-button
         :label (concat (if (char-displayable-p #x1f5d9) "🗙 " "") "Disconnect")
         :action #'pgmacs-disconnect
         :help-echo "Close this connection to PostgreSQL"))
  "List of shortcut buttons to display on the main table-list buffer.")

(defvar pgmacs-row-list-buttons
  (list (pgmacs-shortcut-button
         :label "Export table to CSV buffer"
         :condition (lambda () (not (member (pgcon-server-variant pgmacs--con) '(cratedb questdb ydb materialize risingwave))))
         :action #'pgmacs--table-to-csv
         :help-echo  "Export this table to a CSV buffer")
        (pgmacs-shortcut-button
         :label "Export table to MD buffer"
         :condition (lambda () (not (member (pgcon-server-variant pgmacs--con) '(cratedb questdb ydb materialize risingwave))))
         :action #'pgmacs--table-to-md
         :help-echo  "Export this table to a Markdown buffer")
        ;; Materialize does not support ALTER TABLE ADD COLUMN. Spanner does not allow the addition
        ;; of primary keys with ALTER TABLE. CrateDB does not allow a primary key column to be added
        ;; to a table that is not empty.
        (pgmacs-shortcut-button
         :condition (lambda () (and (not (member (pgcon-server-variant pgmacs--con) '(materialize spanner cratedb risingwave)))
                               (null (pgmacs--table-primary-keys pgmacs--con pgmacs--table))))
         :label "Add primary key to table"
         :action #'pgmacs--add-primary-key
         :help-echo "Add a PRIMARY KEY to enable editing")
        (pgmacs-shortcut-button
         :label (concat (if (char-displayable-p #x2211) "∑ " "") "Count rows")
         :action #'pgmacs--run-count
         :help-echo "Count rows in this table")
        (pgmacs-shortcut-button
         ;; CrateDB only supports ANALYZE on the whole database, not on a single table.
         :condition (lambda () (not (member (pgcon-server-variant pgmacs--con) '(cratedb questdb ydb materialize spanner risingwave))))
         :label (concat (if (char-displayable-p #x1f48a) "💊 " "") "ANALYZE this table")
         :action #'pgmacs--run-analyze
         :help-echo "Run ANALYZE on this table")
        (pgmacs-shortcut-button
         :condition (lambda ()
                      (unless (memq (pgcon-server-variant pgmacs--con) '(cratedb ydb spanner questdb))
                        (null (pg-table-comment pgmacs--con pgmacs--table))))
         :label (format "%sAdd table comment" (or (pgmacs--maybe-svg-icon #'pgmacs--svg-icon-comment) ""))
         :action (lambda (&rest _ignore)
                   (let ((comment (read-from-minibuffer "Table comment: ")))
                     (setf (pg-table-comment pgmacs--con pgmacs--table) comment))
                   (pgmacs--display-table pgmacs--table))
         :help-echo "Add an SQL comment to the table")))

(defun pgmacs--widget-setup ()
  "Set up the appearance of widgets used in PGmacs.
Uses customizations implemented in Emacs' customize support."
  ;; cus-edit.el has this rather rude keymap binding which adds to widget-field-keymap
  ;; bindings that are not relevant to us. So we revert it back to widget-field-keymap.
  (widget-put (get 'editable-field 'widget-type) :keymap widget-field-keymap)
  (setq-local widget-button-face custom-button
	      widget-button-pressed-face custom-button-pressed
	      widget-mouse-face custom-button-mouse)
  (when custom-raised-buttons
    (setq-local widget-push-button-prefix ""
		widget-push-button-suffix ""
		widget-link-prefix ""
		widget-link-suffix "")))

(defvar-keymap pgmacs-table-list-map
  :doc "Keymap for PGmacs table-list buffers"
  "TAB"   #'pgmacs--next-item
  "h"     #'pgmacs--table-list-help
  "?"     #'pgmacs--table-list-help
  "g"     #'pgmacs--table-list-redraw
  "o"     #'pgmacs-open-table
  "p"     #'pgmacs--display-procedures
  "e"     #'pgmacs-run-sql
  "E"     #'pgmacs-run-buffer-sql
  "="     #'pgmacs--shrink-columns
  "r"     #'pgmacs--redraw-pgmacstbl
  "T"     #'pgmacs--switch-to-database-buffer
  "q"     #'bury-buffer)

(defvar-keymap pgmacs-table-list-map/table
  :doc "Keymap for PGmacs table-list buffers when point is in a table"
  :parent pgmacs-table-list-map
  "RET"          #'pgmacs--table-list-RET
  "<deletechar>" #'pgmacs--table-list-delete
  "S"            #'pgmacs--schemaspy-database
  "R"            #'pgmacs--table-list-rename
  "j"            #'pgmacs--row-as-json
  "v"            #'pgmacs--view-value
  ;; "n" and "p" are bound when table is paginated to next/prev page
  "<" (lambda (&rest _ignored)
        (text-property-search-backward 'pgmacstbl)
        (forward-line))
  ">" (lambda (&rest _ignored)
        (text-property-search-forward 'pgmacstbl)
        (forward-line -1))
  "0" (lambda (&rest _ignored) (pgmacstbl-goto-column 0))
  "1" (lambda (&rest _ignored) (pgmacstbl-goto-column 1))
  "2" (lambda (&rest _ignored) (pgmacstbl-goto-column 2))
  "3" (lambda (&rest _ignored) (pgmacstbl-goto-column 3))
  "4" (lambda (&rest _ignored) (pgmacstbl-goto-column 4))
  "5" (lambda (&rest _ignored) (pgmacstbl-goto-column 5))
  "6" (lambda (&rest _ignored) (pgmacstbl-goto-column 6))
  "7" (lambda (&rest _ignored) (pgmacstbl-goto-column 7))
  "8" (lambda (&rest _ignored) (pgmacstbl-goto-column 8))
  "9" (lambda (&rest _ignored) (pgmacstbl-goto-column 9)))



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
 - browse/edit a table (type `\\[pgmacs--table-list-RET]' on the table name or `\\[pgmacs-open-table]' to be
   prompted for a table name in the minibuffer)
 - delete a table (type `\\[pgmacs--table-list-delete]' on the table name)
 - rename a table (type `\\[pgmacs--table-list-rename]' on the table name)
 - modify the SQL comment on a table (type `RET' in the `comment' column)
 - show the output from an SQL query in table mode (type `\\[pgmacs-run-sql]' to enter the
   SQL query in the minibuffer)
 - run SchemaSpy on the database to view its structure (type `\\[pgmacs--schemaspy-database]', only
   available in graphical mode)
 - type `h' to show buffer-specific help and keybindings

In a row-list buffer, which displays the rows of data in a
database table along with metainformation on the table (column
types and associated SQL constraints, on-disk size, table owner),
you can:
 - browse the table contents row by row, in paginated mode for large
   tables. Type `\\[pgmacs--paginated-next]' and `\\[pgmacs--paginated-prev]' to move to the next/previous page in a
   paginated buffer.
 - use `M-right' and `M-left' to move to the next/previous column,
   type a number to move to that column (numbering is zero-based)
 - edit the data value at point (type `\\[pgmacs--row-list-dwim]' on the value you want to
   modify to edit it in the minibuffer, or `\\[pgmacs--edit-value-widget]' to open a dedicated
   widget-based editing buffer)
 - insert a new row by typing `\\[pgmacs--insert-row]' (you will be prompted in the minibuffer
   for new values, unless an SQL default value is defined) or by typing
   `\\[pgmacs--insert-row-widget]' (opens a dedicated widget-based buffer for you to enter the new
   values).
 - copy the current row to the kill ring in JSON format (type `\\[pgmacs--row-as-json]' on the
   row you want to serialize to JSON)
 - delete a row (type `\\[pgmacs--row-list-delete-row]' on the row you wish to delete)
 - copy/paste rows of a database table (type `\\[pgmacs--copy-row]' to copy, `\\[pgmacs--yank-row]' to paste)
 - export the contents of a table to CSV using a dedicated button
 - type `\\[pgmacs--schemaspy-table]' to run SchemaSpy on the current table and display its structure
 - type `\\[pgmacs-open-table]' to open a new row-list buffer for another table
 - type `\\[pgmacs--switch-to-database-buffer]' to jump back to the main table-list buffer
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
  "TAB"   #'pgmacs--next-item
  "q"     #'bury-buffer
  "h"     #'pgmacs--row-list-help
  "?"     #'pgmacs--row-list-help
  "i"     #'pgmacs--insert-row-empty
  "o"     #'pgmacs-open-table
  ;; pgmacs--redraw-pgmacstbl does not refetch data from PostgreSQL;
  ;; pgmacs--row-list-redraw does refetch.
  "r"     #'pgmacs--redraw-pgmacstbl
  "g"     #'pgmacs--row-list-redraw
  "e"     #'pgmacs-run-sql
  "E"     #'pgmacs-run-buffer-sql
  "W"     #'pgmacs--add-where-filter
  "S"     #'pgmacs--schemaspy-table
  "T"     #'pgmacs--switch-to-database-buffer)

;; Additional keybindings for a row-list buffer when the point is inside the table that displays
;; row data.
;;
;; FIXME or perhaps just fix the additional functions to ding when outside of the table?
(defvar-keymap pgmacs-row-list-map/table
  :doc "Keymap for PGmacs row-list buffers when point is in a table"
  :parent pgmacs-row-list-map
  "RET"          #'pgmacs--row-list-dwim
  "w"            #'pgmacs--edit-value-widget
  "!"            #'pgmacs--shell-command-on-value
  "&"            #'pgmacs--async-command-on-value
  "M-u"          #'pgmacs--upcase-value
  "M-l"          #'pgmacs--downcase-value
  "M-c"          #'pgmacs--capitalize-value
  (kbd "v")      #'pgmacs--view-value
  "<delete>"     #'pgmacs--row-list-delete-row
  "<deletechar>" #'pgmacs--row-list-delete-row
  "<backspace>"  #'pgmacs--row-list-delete-row
  "DEL"          #'pgmacs--row-list-delete-row
  "<backtab>"    #'pgmacstbl-previous-column
  (kbd "R")      #'pgmacs--row-list-rename-column
  (kbd "+")      #'pgmacs--insert-row
  (kbd "i")      #'pgmacs--insert-row-widget
  (kbd "k")      #'pgmacs--copy-row
  (kbd "y")      #'pgmacs--yank-row
  (kbd "=")      #'pgmacs--shrink-columns
  (kbd "j")      #'pgmacs--row-as-json
  (kbd "d")      #'pgmacs--row-list-mark-row
  (kbd "u")      #'pgmacs--row-list-unmark-row
  (kbd "U")      #'pgmacs--row-list-unmark-all
  (kbd "x")      #'pgmacs--row-list-delete-marked
  ;; "n" and "p" are bound when table is paginated to next/prev page
  (kbd "<")  (lambda (&rest _ignored)
               (text-property-search-backward 'pgmacstbl)
               (forward-line))
  (kbd ">")  (lambda (&rest _ignored)
               (text-property-search-forward 'pgmacstbl)
               (forward-line -1))
  (kbd "0") (lambda (&rest _ignored) (pgmacstbl-goto-column 0))
  (kbd "1") (lambda (&rest _ignored) (pgmacstbl-goto-column 1))
  (kbd "2") (lambda (&rest _ignored) (pgmacstbl-goto-column 2))
  (kbd "3") (lambda (&rest _ignored) (pgmacstbl-goto-column 3))
  (kbd "4") (lambda (&rest _ignored) (pgmacstbl-goto-column 4))
  (kbd "5") (lambda (&rest _ignored) (pgmacstbl-goto-column 5))
  (kbd "6") (lambda (&rest _ignored) (pgmacstbl-goto-column 6))
  (kbd "7") (lambda (&rest _ignored) (pgmacstbl-goto-column 7))
  (kbd "8") (lambda (&rest _ignored) (pgmacstbl-goto-column 8))
  (kbd "9") (lambda (&rest _ignored) (pgmacstbl-goto-column 9)))


(defvar-keymap pgmacs-proc-list-map
  :doc "Keymap for PGmacs proc-list buffers"
  "h"            #'pgmacs--proc-list-help
  "?"            #'pgmacs--proc-list-help
  "RET"          #'pgmacs--proc-list-RET
  "TAB"          #'pgmacs--next-item
  "<deletechar>" #'pgmacs--proc-list-delete
  "T"            #'pgmacs--switch-to-database-buffer
  "R"            #'pgmacs--proc-list-rename
  ;; "g" pgmacs--proc-list-redraw
  ;; the functions pgmacstbl-beginning-of-table and
  ;; pgmacstbl-end-of-table don't work when we have inserted text before
  ;; the pgmacstbl.
  "<" (lambda (&rest _ignored)
        (text-property-search-backward 'pgmacstbl)
        (forward-line))
  ">" (lambda (&rest _ignored)
        (text-property-search-forward 'pgmacstbl)
        (forward-line -1))
  "q" (lambda (&rest _ignored) (bury-buffer)))


(defvar-keymap pgmacs-transient-map
  :doc "Keymap for PGmacs transient buffers"
  "q"  #'bury-buffer
  "o"  #'pgmacs-open-table
  "e"  #'pgmacs-run-sql
  "E"  #'pgmacs-run-buffer-sql
  "T"  #'pgmacs--switch-to-database-buffer)

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
                                    (progress-reporter-update pgmacs--progress)))))
  (run-with-timer 10 nil #'pgmacs--stop-progress-reporter))

(defun pgmacs--update-progress (msg)
  "Update the progress reporter with message MSG."
  (when pgmacs--progress
    (progress-reporter-update msg)))

(defun pgmacs--stop-progress-reporter ()
  "Stop the progress reporter."
  (when pgmacs--progress
    (message "Stopping the PGmacs progress reporter")
    (progress-reporter-done pgmacs--progress))
  (when pgmacs--progress-timer
    (cancel-timer pgmacs--progress-timer)
    (setq pgmacs--progress-timer nil))
  (when pgmacs--progress
    (setq pgmacs--progress nil)))


(defvar-local pgmacs--kill-ring nil
  "Used for copying and pasting rows in a buffer's table.")

(defvar pgmacs--column-display-functions (make-hash-table :test #'equal))

;; Allow the user to register a dedicated display function for a particular column in a particular
;; table. The display-function takes three arguments: the cell-value, max-width, table.
;;
;; This functionality can be used to display BYTEA columns as inline images, for example.
(defun pgmacs-register-column-displayer (table column display-function)
  (puthash (cons table column) display-function pgmacs--column-display-functions))

(defun pgmacs--lookup-column-displayer (table column)
  (gethash (cons table column) pgmacs--column-display-functions nil))


(defun pgmacs-flush-table (con table)
  "Force database to make recent insertions to TABLE visible.
This is needed for certain distributed PostgreSQL variants that provide
eventually-consistent semantics (at least CrateDB and RisingWave)."
  (pcase (pgcon-server-variant con)
    ('cratedb
     (pg-exec con (format "REFRESH TABLE %s" (pg-escape-identifier table))))
    ('risingwave
     (pg-exec con "FLUSH"))))

(defun pgmacs-disconnect (&rest _ignore)
  (unless pgmacs--con
    (user-error "This function must be called from a PGmacs buffer"))
  (pgmacs--stop-progress-reporter)
  (pg-disconnect pgmacs--con)
  (setq pgmacs--con nil)
  (kill-buffer (current-buffer)))

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
         (lambda (val) (format-time-string pgmacs-timestamp-format val pgmacs-timestamp-zone)))
        ((or (string= type-name "text")
             (string= type-name "varchar")
             (string= type-name "name"))
         (lambda (s) (or s "")))
        ((string= type-name "bpchar")
         (lambda (val)
           (if (eql 1 (length val))
               (char-to-string val)
             val)))
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
  (pcase type-name
    ("smallint" 4)
    ("int2" 4)
    ("int4" 6)
    ("int8" 10)
    ("oid" 10)
    ("bool" 4)
    ("bit" 4)
    ("varbit" 10)
    ("char" 4)
    ("char2" 4)
    ("char4" 6)
    ("char8" 10)
    ("char16" 20)
    ("text" 25)
    ("varchar" 25)
    ("bpchar" 7)
    ("name" 25)
    ("bytea" 10)
    ("json" 40)
    ("jsonb" 40)
    ("uuid" 36)
    ("hstore" 20)
    ("numeric" 10)
    ("float4" 10)
    ("float8" 10)
    ("date" 16)
    ("timestamp" 20)
    ("timestamptz" 20)
    ("datetime" 20)
    ("time" 12)
    ("reltime" 10)
    ("timespan" 12)
    (_ 10)))

(defun pgmacs--alignment-for (type-name)
  "Return the alignment for type TYPE-NAME, either \='left or \='right."
  (pcase type-name
    ("smallint" 'right)
    ("int2" 'right)
    ("int4" 'right)
    ("int8" 'right)
    ("oid" 'right)
    ("bool" 'left)
    ("bit" 'right)
    ("varbit" 'right)
    (_ 'left)))

;; Name may be a qualified name or a simple string. Transform this into a string for display to the
;; user. We only want to show the outer ?" characters if they are necessary (if some characters in
;; the identifier require quoting).
(defun pgmacs--display-identifier (name)
  "Return PostgreSQL identifier NAME in a form suitable for display to user."
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

(cl-defun pgmacs--row-as-json (&rest _ignore)
  "Copy the current row as JSON to the kill ring."
  (interactive)
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
           (current-row (pgmacstbl-current-object))
           (ht (make-hash-table :test #'equal))
           (ce (pgcon-client-encoding pgmacs--con)))
      (cl-loop
       for col-id from 0
       for col in cols
       for col-type = (aref pgmacs--column-type-names col-id)
       for raw in current-row
       for v = (cond ((jsonable-p raw) raw)
                     ((null raw) nil)
                     (t
                      (pg-serialize raw col-type ce)))
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
  (when (null primary-keys)
    (user-error "Can't edit content of a table that has no PRIMARY KEY"))
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
      (unless pk-value
        (error "Can't find value for primary key %s" pk))
      (let* ((sql (format "UPDATE %s SET %s = $1 WHERE %s = $2"
                          (pg-escape-identifier pgmacs--table)
                          (pg-escape-identifier col-name)
                          (pg-escape-identifier pk)))
             (res (pg-exec-prepared pgmacs--con sql
                                    `((,new-value . ,col-type)
                                      (,pk-value . ,pk-col-type)))))
        (pgmacs-flush-table pgmacs--con pgmacs--table)
        (pgmacs--notify "%s" (pg-result res :status)))
      (let ((new-row (copy-sequence current-row)))
        (setf (nth col-id new-row) new-value)
        (pgmacstbl-update-object pgmacstbl new-row current-row)
        ;; redrawing is necessary to ensure that all keybindings are present for the newly inserted
        ;; row.
        (forward-line -1)
        (pgmacs--redraw-pgmacstbl))
      new-value)))

(defun pgmacs--edit-value-minibuffer (row)
  "Edit and update in PostgreSQL the column value at point.
The new value in database row ROW is read in the minibuffer.
Editing requires the database table to have a primary key."
  (unless pgmacs--table-primary-keys
    (user-error "Cannot edit a table that has no PRIMARY KEY"))
  (let ((get-value (lambda (old-value col-name col-type)
                     (pgmacs--read-value (substring-no-properties col-name)
                                         (substring-no-properties col-type)
                                         "Change %s (%s) to: "
                                         old-value))))
  (pgmacs--setf-cell row pgmacs--table-primary-keys get-value)))


(defvar pgmacs--shell-command-history nil)

(defun pgmacs--shell-command-on-value (&rest _ignore)
  "Run a Unix filter shell command with the current cell value as input.

When called without a prefix argument, output is diplayed in the
echo area. When called with a prefix argument, replace the
current cell value with the output
(updating the database).

For example, to count the number of characters in the current cell,

   ! wc -c

To downcase the value of a text cell (and modify the value in the
database) use

   C-u ! tr '[:upper:]' '[:lower]'

To reverse the order of the characters in the cell (and modify
the value in the database), use

   C-u ! rev

Operates on the current row. The table must have a primary key."
  (interactive)
  (when (null pgmacs--table-primary-keys)
    (user-error "Can't edit content of a table that has no PRIMARY KEY"))
  (let ((current-row (pgmacstbl-current-object))
        (get-value
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
      (pgmacs--setf-cell current-row pgmacs--table-primary-keys get-value))))

(defvar pgmacs--async-command-history nil)

(defun pgmacs--async-command-on-value (&rest _ignore)
  "Run a command asynchronously with the current cell value as first argument.

The command should be the name of a program, which will be
searched for in `exec-path' (it is run via `start-process',
without a shell). Command output will be displayed in a buffer
called *PGmacs async command*.

For example, if the cell contains a filename, you can open the
filename in the default application on your system by entering
`xdg-open' (or `open' on a MacOS machine).

Operates on the current row. The table must have a primary key."
  (interactive)
  (pgmacs-funcall-cell
   (lambda (cell-value)
     (let* ((prompt "Async command: ")
            (cmd (read-string prompt nil 'pgmacs--async-command-history))
            (buf (get-buffer-create "*PGmacs async command*")))
       (start-process "PGmacs-async-command" buf cmd cell-value)))))

(defun pgmacs--downcase-value (&rest _ignore)
  "Downcase the value in the cell at point and update PostgreSQL.
Operates on the current row. The table must have a primary key."
  (interactive)
  (when (null pgmacs--table-primary-keys)
    (user-error "Can't edit content of a table that has no PRIMARY KEY"))
  (let ((current-row (pgmacstbl-current-object))
        (get-value
         (lambda (old-value _col-name col-type)
           (unless (or (string= "text" col-type)
                       (string= "varchar" col-type)
                       (string= "bpchar" col-type)
                       (string= "name" col-type))
             (user-error "Can only downcase text values"))
           (with-temp-buffer
             (insert old-value)
             (downcase-region (point-min) (point-max))
             (buffer-substring-no-properties (point-min) (point-max))))))
    (pgmacs--setf-cell current-row pgmacs--table-primary-keys get-value)))

(defun pgmacs--upcase-value (&rest _ignore)
  "Upcase the value in the cell at point and update PostgreSQL.
Operates on the CURRENT-ROW. The table must have a primary key."
  (interactive)
  (when (null pgmacs--table-primary-keys)
    (user-error "Can't edit content of a table that has no PRIMARY KEY"))
  (let ((current-row (pgmacstbl-current-object))
        (get-value
         (lambda (old-value _col-name col-type)
           (unless (or (string= "text" col-type)
                       (string= "varchar" col-type)
                       (string= "bpchar" col-type)
                       (string= "name" col-type))
             (user-error "Can only upcase text values"))
           (with-temp-buffer
             (insert old-value)
             (upcase-region (point-min) (point-max))
             (buffer-substring-no-properties (point-min) (point-max))))))
    (pgmacs--setf-cell current-row pgmacs--table-primary-keys get-value)))

(defun pgmacs--capitalize-value (&rest _ignore)
  "Capitalize the value in the cell at point and update PostgreSQL.
Operates on the current row. The table must have a primary key."
  (interactive)
  (when (null pgmacs--table-primary-keys)
    (user-error "Can't edit content of a table that has no PRIMARY KEY"))
  (let ((current-row (pgmacstbl-current-object))
        (get-value
         (lambda (old-value _col-name col-type)
           (unless (or (string= "text" col-type)
                       (string= "varchar" col-type)
                       (string= "bpchar" col-type)
                       (string= "name" col-type))
             (user-error "Can only capitalize text values"))
           (with-temp-buffer
             (insert old-value)
             (capitalize-region (point-min) (point-max))
             (buffer-substring-no-properties (point-min) (point-max))))))
    (pgmacs--setf-cell current-row pgmacs--table-primary-keys get-value)))


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
  :args `((editable-list :inline t
                         ;; don't display the [INS] button on each line (which would be inserted if
                         ;; the %i escape were present here), because the order of entries in the
                         ;; HSTORE is not relevant.
                         :entry-format "%d %v\n"
                         (cons :format "%v"
                               (editable-field :size 25
                                                :tag "Key"
                                                :format ,(if (char-displayable-p ?⟶) "%v ⟶ " "%v --> "))
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

(define-widget 'pgmacs-timestamp-widget 'text
  "Widget to edit a timestamp."
  :tag "Timestamp"
  :format "%v"
  :offset 2
  :value-to-internal (lambda (_widget val)
                       (if (stringp val)
                           val
                         (format-time-string pgmacs-timestamp-format val)))
  :value-to-external (lambda (_widget str)
                       (pg-isodate-without-timezone-parser str nil))
  :args '((string :inline t :size 25)))

(define-widget 'pgmacs-timestamptz-widget 'text
  "Widget to edit a timestamptz."
  :tag "Timestamptz"
  :format "%v"
  :offset 2
  :value-to-internal (lambda (_widget val)
                       (if (stringp val)
                           val
                         (format-time-string pgmacs-timestamp-format val)))
  :value-to-external (lambda (_widget str)
                       (pg-isodate-with-timezone-parser str nil))
  :args '((string :inline t :size 25)))

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
  (pcase type
    ("bool"
     (widget-create 'boolean (or current-value nil)))
    ((or "smallint" "int2" "int4" "int8" "oid")
     (widget-create 'integer (or current-value "")))
    ((or "numeric" "float4" "float8")
     (widget-create 'float (or current-value "")))
    ("char"
     (widget-create 'character (or current-value "")))
    ((or "text" "varchar" "bpchar")
     (let ((default (or current-value "")))
       (widget-create 'string
                      :size (max 80 (min 200 (+ 5 (length default))))
                      :value default)))
    ;; represented as "[44,33,5,78]" on the wire. Parsed to an elisp vector of integers.
    ("vector"
     (widget-create '(vector integer) :value (or current-value (vector))))
    ("hstore"
     (widget-create 'pgmacs-hstore-widget :value (or current-value (make-hash-table))))
    ((or "json" "jsonb")
     (widget-create 'pgmacs-json-widget :value (or current-value (make-hash-table))))
    ("date"
     (widget-create 'pgmacs-date-widget (or current-value "")))
    ;; A timestamp and timestamptz is represented as an elisp timestamp, which is (v29) a cons of
    ;; two integers. We want to edit it as a timestamp string.
    ("timestamp"
     (widget-create 'pgmacs-timestamp-widget :value (or current-value (current-time-string))))
    ("timestamptz"
     (widget-create 'pgmacs-timestamptz-widget :value (or current-value (current-time-string))))
    ("uuid"
     (widget-create 'pgmacs-uuid-widget :value (or current-value "")
                    :action (lambda (wid &rest _ignore)
                              (if (widget-apply wid :validate)
                                  (user-error "Invalid UUID: %s" (widget-get wid :error))
                                (message "%s is ok" (widget-value wid))))))
    (_
     (let ((current-string (if (stringp current-value)
                               current-value
                             (format "%s" current-value))))
       (widget-create 'editable-field
                      :size (min 200 (+ 5 (length current-string)))
                      (format "%s" current-value))))))

(defun pgmacs--edit-value-widget (&rest _ignored)
  "Edit and update in PostgreSQL the value at point in ROW.
Uses a dedicated widget buffer.  Editing is only possible if the current table
has a primary key."
  (interactive)
  (when (null pgmacs--table-primary-keys)
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
         (pk (cl-first pgmacs--table-primary-keys))
         (pk-col-id (cl-position pk cols :key #'pgmacstbl-column-name :test #'string=))
         (pk-col-type (aref pgmacs--column-type-names pk-col-id))
         (pk-value (and pk-col-id (nth pk-col-id current-row))))
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
                        (pgmacs-flush-table con pgmacs--table)
                        (pgmacs--notify "%s" (pg-result res :status))
                        (let ((new-row (copy-sequence current-row)))
                          (setf (nth col-id new-row) new-value)
                          (pgmacstbl-update-object pgmacstbl new-row current-row)
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
                                               (if (char-displayable-p ?🐘) " 🐘 " "")
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

(defun pgmacs--view-value (&rest _ignore)
  "Insert column value at point into a dedicated buffer."
  (interactive)
  (let* ((db-buffer pgmacs--db-buffer)
         (col-id (pgmacstbl-current-column))
         (cols (pgmacstbl-columns (pgmacstbl-current-table)))
         (col (nth col-id cols))
         (col-name (pgmacstbl-column-name col))
         (current-row (pgmacstbl-current-object))
         (buf (get-buffer-create (format "*PostgreSQL column value %s*" col-name)))
         (value (funcall (pgmacstbl-column-formatter col)
                         (nth col-id current-row))))
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

(defun pgmacs--row-list-delete-row (&rest _ignore)
  "Delete the row at point from the database table.
Deletion is only possible when the table has primary key."
  (interactive)
  (when (null pgmacs--table-primary-keys)
    (user-error "Can't edit content of a table that has no PRIMARY KEY"))
  (let* ((pgmacstbl (pgmacstbl-current-table))
         (cols (pgmacstbl-columns pgmacstbl))
         (current-row (pgmacstbl-current-object))
         (where-clauses (list))
         (where-values (list))
         (counter 0))
    (when (y-or-n-p (format "Really delete PostgreSQL row %s?" current-row))
      (dolist (pk pgmacs--table-primary-keys)
        (let* ((col-name (cl-position pk cols :key #'pgmacstbl-column-name :test #'string=))
               (col-type (and col-name (aref pgmacs--column-type-names col-name)))
               (value (and col-name (nth col-name current-row))))
          (unless value
            (error "Can't find value for primary key %s" pk))
          (push (format "%s = $%d" (pg-escape-identifier pk) (cl-incf counter)) where-clauses)
          (push (cons value col-type) where-values)))
      (setq where-clauses (nreverse where-clauses)
            where-values (nreverse where-values))
      (pg-exec pgmacs--con "START TRANSACTION")
      (unwind-protect
          (condition-case e
              (let* ((sql (format "DELETE FROM %s WHERE %s"
                                  (pg-escape-identifier pgmacs--table)
                                  (string-join where-clauses " AND ")))
                     (res (pg-exec-prepared pgmacs--con sql where-values))
                     (status (pg-result res :status)))
                (pgmacs-flush-table pgmacs--con pgmacs--table)
                (pgmacs--notify "%s" status)
                (unless (string-prefix-p "DELETE " status)
                  (error "Unexpected status %s for PostgreSQL DELETE command" status))
                (let ((rows (cl-parse-integer (substring status 7))))
                  (cond ((eql 0 rows)
                         (display-warning 'pgmacs "Could not delete PostgreSQL row")
                         (pg-exec pgmacs--con "COMMIT TRANSACTION"))
                        ((eql 1 rows)
                         (pg-exec pgmacs--con "COMMIT TRANSACTION")
                         (pgmacstbl-remove-object pgmacstbl current-row)
                         (pgmacs--redraw-pgmacstbl))
                        (t
                         (display-warning 'pgmacs "Deletion affected more than 1 row; rolling back")
                         (pg-exec pgmacs--con "ROLLBACK TRANSACTION")))))
            (pg-error
             (message "Couldn't delete row: %s" e)))
          (pg-exec pgmacs--con "ROLLBACK TRANSACTION")))))

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
      (pgmacs-flush-table pgmacs--con pgmacs--table)
      (pgmacs--notify "%s" (pg-result res :status))
      ;; It's tempting to use pgmacstbl-insert-object here to avoid a full refresh of the pgmacstbl.
      ;; However, we don't know what values were chosen for any columns that have a default, so we
      ;; need to refetch the data from PostgreSQL.
      (setq pgmacs--marked-rows (list))
      (pgmacs--display-table pgmacs--table))))


(defun pgmacs--insert-row (&rest _ignore)
  "Insert a new row of data into the current table after point.
Uses the minibuffer to prompt for new values."
  ;; TODO we need to handle the case where there is no existing pgmacstbl because the underlying SQL
  ;; table is empty.
  (interactive)
  (let* ((pgmacstbl (pgmacstbl-current-table))
         (cols (pgmacstbl-columns pgmacstbl))
         (current-row (pgmacstbl-current-object))
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
      (pgmacs-flush-table pgmacs--con pgmacs--table)
      (pgmacs--notify "%s" (pg-result res :status))
      ;; It's tempting to use pgmacstbl-insert-object here to avoid a full refresh of the pgmacstbl.
      ;; However, we don't know what values were chosen for any columns that have an SQL default, so
      ;; we need to refetch the data from PostgreSQL.
      (setq pgmacs--marked-rows (list))
      (pgmacs--display-table pgmacs--table))))

(defun pgmacs--insert-row-widget (&rest _ignore)
  "Insert a new row of data into the current table after point.
Uses a widget-based buffer to prompt for new values.  Updates the
PostgreSQL database."
  (interactive)
  (let* ((con pgmacs--con)
         (db-buffer pgmacs--db-buffer)
         (table pgmacs--table)
         (ce (pgcon-client-encoding con))
         (pgmacstbl (pgmacstbl-current-table))
         (current-row (pgmacstbl-current-object))
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
                        (pgmacs-flush-table pgmacs--con pgmacs--table)
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

(defun pgmacs--copy-row (&rest _ignore)
  "Copy the current row to the PGmacs internal kill ring."
  (interactive)
  (setq pgmacs--kill-ring (cons pgmacs--table (pgmacstbl-current-object)))
  (message "Row copied to PGmacs kill ring"))

;; Insert new row at current position based on content of our "kill ring".
(defun pgmacs--yank-row (&rest _ignore)
  "Insert a new row into the current table after the row at point.
The new row contents are based on the last copied row. Columns for which
a default SQL value is defined (such as a SERIAL type) will take the
default value instead of the last copied value.
Updates the PostgreSQL database."
  (interactive)
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
      ;; However, we don't know what values were chosen for any columns that have an SQL default.
      ;; This means that we can't insert at the current-row position.
      (setq pgmacs--marked-rows (list))
      ;; Redisplay the table, but attempt to put point on the new row
      (let ((pos (point)))
        (pgmacs--display-table pgmacs--table)
        (when (< pos (point-max))
          (goto-char pos)
          (recenter))))))

;; This SQL query adapted from https://stackoverflow.com/a/20537829
(defun pgmacs--table-primary-keys/full (con table)
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

(defun pgmacs--table-primary-keys (con table)
  "Return the columns active as PRIMARY KEY in TABLE.
Uses PostgreSQL connection CON."
  (pcase (pgcon-server-variant con)
    ('questdb nil)
    ('cratedb nil)
    ('spanner nil)
    ('ydb nil)
    (_ (pgmacs--table-primary-keys/full con table))))

(defun pgmacs--table-indexes/full (con table)
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

(defun pgmacs--table-indexes (con table)
  "Return details of the indexes present on TABLE.
Uses PostgreSQL connection CON."
  (pcase (pgcon-server-variant con)
    ('cratedb nil)
    ('questdb nil)
    ('ydb nil)
    ('materialize nil)
    ('spanner nil)
    ('risingwave nil)
    (_ (pgmacs--table-indexes/full con table))))

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

(defun pgmacs--fk-constraints (con table)
  "Return the list of foreign key constraints for TABLE.
Uses PostgreSQL connection CON."
  (pcase (pgcon-server-variant con)
    ((or 'postgresql 'orioledb 'ivorysql 'timescaledb 'citusdb 'xata)
     (let* ((t-id (pg-escape-identifier table))
            ;; Query adapted from https://stackoverflow.com/questions/1152260/how-to-list-table-foreign-keys
            (sql "SELECT conname,
                pg_catalog.pg_get_constraintdef(r.oid, true) as condef
               FROM pg_catalog.pg_constraint r
               WHERE r.conrelid = $1::regclass AND r.contype = 'f' ORDER BY 1")
            (argument-types (list "text"))
            (params `((,t-id . "text")))
            (ps-name (pg-ensure-prepared-statement con "QRY-table-fk-constraints" sql argument-types))
            (res (pg-fetch-prepared con ps-name params)))
       (pg-result res :tuples)))
    (_ nil)))

;; Function pgmacs--column-info uses some moderately complex SQL queries to determine the
;; constraints of a column. These queries are called once per column for a row-list buffer. To avoid
;; redundant processing by PostgreSQL in parsing and preparing a query plan for these queries, we
;; use PostgreSQL prepared statements via the `pg-ensure-prepared' function.
(defun pgmacs--column-info/full (con table column)
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
                 $1,
                 cl.relname AS parent_table,
                 att.attname AS parent_column,
                 conname
               FROM
                (SELECT
                     unnest(con1.conkey) AS parent,
                     unnest(con1.confkey) AS child,
                     con1.confrelid,
                     con1.conrelid,
                     con1.conname
                 FROM
                     pg_class cl
                     JOIN pg_namespace ns ON cl.relnamespace = ns.oid
                     JOIN pg_constraint con1 ON con1.conrelid = cl.oid
                 WHERE
                     ns.nspname = $1 AND
                     cl.relname = $2 AND
                     con1.contype = 'f'
                ) con
               JOIN pg_attribute att ON
                    att.attrelid = con.confrelid and att.attnum = con.child
               JOIN pg_class cl ON
                    cl.oid = con.confrelid
               JOIN pg_attribute att2 ON
                    att2.attrelid = con.conrelid and att2.attnum = con.parent
               WHERE att2.attname = $3")
         ;; this query has the same arguments (and argument types) as that above
         (ps-name (pg-ensure-prepared-statement con "QRY-references-constraints" sql argument-types))
         (res (pg-fetch-prepared con ps-name params))
         (references-constraints (pg-result res :tuples))
         (sql "SELECT character_maximum_length FROM information_schema.columns
               WHERE table_schema=$1 AND table_name=$2 AND column_name=$3")
         (ps-name (pg-ensure-prepared-statement con "QRY-maxlen" sql argument-types))
         (res (pg-fetch-prepared con ps-name params))
         (maybe-maxlen (cl-first (pg-result res :tuple 0)))
         (maxlen (if (equal pg-null-marker maybe-maxlen) nil maybe-maxlen))
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
    (puthash "TYPE" (propertize type-name 'help-echo "The type of this column") column-info)
    (dolist (c check-constraints)
      (cond ((string= "CHECK" (cl-first c))
             (let* ((sql "SELECT check_clause FROM information_schema.check_constraints
                          WHERE constraint_schema=$1 and constraint_name=$2")
                    (res (pg-exec-prepared con sql `((,schema . "text") (,(cl-second c) . "text"))))
                    (clauses (pg-result res :tuple 0)))
               (puthash (cl-first c) (format "%s %s" (cl-second c) (cl-first clauses)) column-info)))
            (t
             (puthash (propertize (cl-first c) 'text-echo "Constraint type") (cl-second c) column-info))))
    ;; If references-constraints contains a single row, we are in the presence of a simple foreign
    ;; key reference (to a single column), and the second element of the hash-table entry is the
    ;; target column name. If there is more than one row, we have a complex (multi-column) foreign
    ;; key reference, and the second element of the hash-table entry is set to nil.
    (when references-constraints
      (let* ((fc (cl-first references-constraints))
             (target-col (and (eql 1 (length references-constraints))
                              (cl-third fc))))
        (let ((sqn (make-pg-qualified-name :schema (cl-first fc) :name (cl-second fc))))
          (puthash "REFERENCES" (list sqn target-col) column-info))))
    (when (pgmacs--column-nullable-p con table column)
      (puthash (pgmacs--make-badge "NOT NULL" :color "#777" :help-echo "Not null constraint") nil column-info))
    (when  maxlen
      (puthash "maxlen" maxlen column-info))
    (when defaults
      (puthash (propertize "DEFAULT" 'help-echo "Default value for this column") defaults column-info))
    (when-let* ((comment (pg-column-comment con table column))
                (maybe-icon (pgmacs--maybe-svg-icon #'pgmacs--svg-icon-comment))
                (label (propertize (or maybe-icon "COMMENT") 'help-echo "Comment on this column")))
      (puthash label comment column-info))
    column-info))

(defun pgmacs--column-info/basic (con table column)
  (let* ((defaults (pg-column-default con table column))
         (sql (format "SELECT %s FROM %s LIMIT 0"
                      (pg-escape-identifier column)
                      (pg-escape-identifier table)))
         (res (pg-exec con sql))
         (oid (cadar (pg-result res :attributes)))
         (column-info (make-hash-table :test #'equal))
         (type-name (pg-lookup-type-name con oid)))
    (puthash "TYPE" type-name column-info)
    (when defaults
      (puthash "DEFAULT" defaults column-info))
    (when-let* ((comment (pg-column-comment con table column)))
      (puthash "COMMENT" comment column-info))
    column-info))

(defun pgmacs--column-info (con table column)
  "Return a hashtable containing metainformation on COLUMN in TABLE.
The metainformation includes the type name, whether the column is a PRIMARY KEY,
whether it is affected by constraints such as UNIQUE.  Information is retrieved
over the PostgreSQL connection CON."
  (pcase (pgcon-server-variant con)
    ((or 'cratedb 'questdb 'ydb 'materialize 'spanner 'risingwave)
     (pgmacs--column-info/basic con table column))
    (_ (pgmacs--column-info/full con table column))))

;; Format the column-info hashtable as a string for display
(defun pgmacs--format-column-info (column-info)
  (let ((items (list)))
    (maphash (lambda (k v)
               (cond ((string= "TYPE" k) nil)
                     ((string= "REFERENCES" k)
                      (when (cl-second v)
                        (push (format "REFERENCES %s(%s)"
                                      (pgmacs--display-identifier (cl-first v))
                                      (cl-second v))
                              items)))
                     ((string= "PRIMARY KEY" k)
                      (push (concat (pgmacs--make-badge "PRIMARY KEY" :color "#777") " " v) items))
                     ((string= "DEFAULT" k)
                      (push (concat (pgmacs--make-badge "DEFAULT" :color "#777") " " v) items))
                     ((string= "CHECK" k)
                      (push (concat (pgmacs--make-badge "CHECK" :color "#777") " " v) items))
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


;; The count of table rows using COUNT(*) is imperfect for a number of reasons: it's not using a
;; parameterized query (not possible for a DDL query), and it's slow on large tables, requiring a
;; full index or table scan. However, alternatives are not reliable and will return incorrect
;; results for tables that haven't yet been VACCUMed or ANALYZEd.
(defun pgmacs--estimate-row-count/expensive (table)
  (let* ((tid (pg-escape-identifier table))
         (res (pg-exec pgmacs--con (format "SELECT COUNT(*) FROM %s" tid)))
         (tuple (pg-result res :tuple 0)))
    (cl-first tuple)))

;; See discussion at
;; https://stackoverflow.com/questions/7943233/fast-way-to-discover-the-row-count-of-a-table-in-postgresql
(defun pgmacs--estimate-row-count/fast (table)
  (let* ((schema (if (pg-qualified-name-p table)
                     (pg-qualified-name-schema table)
                   "public"))
         (tname (if (pg-qualified-name-p table)
                    (pg-qualified-name-name table)
                  table))
         (sql "SELECT c.reltuples::bigint AS estimate
               FROM   pg_class c
               JOIN   pg_namespace n ON n.oid = c.relnamespace
               WHERE n.nspname = $1 AND c.relname = $2")
         (argument-types (list "text" "text"))
         (params `((,schema . "text") (,tname . "text")))
         (ps-name (pg-ensure-prepared-statement pgmacs--con "QRY-estimate-row-count" sql argument-types))
         (res (pg-fetch-prepared pgmacs--con ps-name params)))
    (cl-first (pg-result res :tuple 0))))

;; If the database size is "large" (according to customizable variable
;; pgmacs-large-database-threshold), avoid index/table scans and use a fast but unreliable method of
;; estimating the row count. Otherwise, use a naïve COUNT(*) query which is expensive but always
;; returns valid results.
(defun pgmacs--estimate-row-count (table)
  (let ((db-size (get 'pgmacs--con 'database-size)))
    (if (and db-size (> db-size pgmacs-large-database-threshold))
        (pgmacs--estimate-row-count/fast table)
      (pgmacs--estimate-row-count/expensive table))))

(defun pgmacs--table-size-ondisk (con table)
  (pcase (pgcon-server-variant con)
    ((or 'postgresql 'yugabyte)
     (let* ((schema (if (pg-qualified-name-p table)
                        (pg-qualified-name-schema table)
                      "public"))
            (tname (if (pg-qualified-name-p table)
                       (pg-qualified-name-name table)
                     table))
            (sql "SELECT
                     pg_catalog.pg_total_relation_size(c.oid)
                   FROM pg_class c
                   JOIN pg_namespace n ON c.relnamespace = n.oid
                   WHERE (n.nspname, c.relname) = ($1, $2)")
            (res (pg-exec-prepared con sql `((,schema . "text") (,tname . "text"))))
            (tuple (pg-result res :tuple 0)))
       (cl-first tuple)))
    ('questdb
     (let* ((sql "SELECT diskSize FROM table_storage() WHERE tableName=$1")
            (res (pg-exec-prepared con sql `((,table . "text"))))
            (tuple (pg-result res :tuple 0)))
       (cl-first tuple)))
    ('cratedb
     (let* ((schema (if (pg-qualified-name-p table)
                        (pg-qualified-name-schema table)
                      (pg-current-schema con)))
            (tname (if (pg-qualified-name-p table)
                       (pg-qualified-name-name table)
                     table))
            (sql "SELECT SUM(size) FROM sys.shards WHERE schema_name=$1 AND table_name=$2")
            (res (pg-exec-prepared con sql `((,schema . "text") (,tname . "text"))))
            (tuple (pg-result res :tuple 0)))
       (cl-first tuple)))
    ('cockroachdb
     ;; CockroachDB doesn't accept a prepared statement for the SHOW RANGES query.
     (let* ((sql "SELECT sum(range_size_mb) FROM [SHOW RANGES FROM TABLE %s WITH DETAILS]")
            (res (pg-exec con (format sql (pg-escape-identifier table))))
            (tuple (pg-result res :tuple 0)))
       (* 1000000 (cl-first tuple))))
    (_ nil)))

(defun pgmacs--index-size-ondisk (con table)
  (pcase (pgcon-server-variant con)
    ('postgresql
     (let* ((schema (if (pg-qualified-name-p table)
                        (pg-qualified-name-schema table)
                      "public"))
            (tname (if (pg-qualified-name-p table)
                       (pg-qualified-name-name table)
                     table))
            (sql "SELECT
                     pg_catalog.pg_indexes_size(c.oid)
                   FROM pg_class c
                   JOIN pg_namespace n ON c.relnamespace = n.oid
                   WHERE (n.nspname, c.relname) = ($1, $2)")
            (res (pg-exec-prepared con sql `((,schema . "text") (,tname . "text"))))
            (tuple (pg-result res :tuple 0)))
       (cl-first tuple)))
    (_ nil)))

(cl-defun pgmacs--show-index-stats (con index-name)
  "Create a buffer with information concerning INDEX-NAME.
Uses PostgreSQL connection CON."
  (interactive)
  (let* ((con pgmacs--con)
         (db-buffer pgmacs--db-buffer)
         (inhibit-read-only t)
         (res (pg-exec con "SELECT 1 FROM pg_extension WHERE extname = 'pgstattuples'"))
         (tuples (pg-result res :tuples)))
    ;; Loading this extension requires superuser or EXECUTE on pg_stat_scan_tables privileges.
    (unless tuples
      (condition-case e
          (pg-exec con "CREATE EXTENSION IF NOT EXISTS pgstattuple")
        (pg-error
         (display-warning 'pgmacs "Could not load PostgreSQL module pgstattuple")
         (cl-return-from pgmacs--show-index-stats nil))))
    (pop-to-buffer (get-buffer-create "*PostgreSQL index information*"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (remove-overlays)
    (kill-all-local-variables)
    (setq-local pgmacs--con con
                pgmacs--db-buffer db-buffer
                buffer-read-only nil)
    (buffer-disable-undo)
    (insert (propertize (format "Statistics for index %s" index-name) 'face 'bold) "\n\n")
    ;; TODO: we should perhaps account for a schema-qualified index-name
    (let* ((res (pg-exec-prepared con "SELECT * FROM pgstatindex($1)" `((,index-name . "text"))))
           (data (pg-result res :tuple 0))
           (column-names (mapcar #'cl-first (pg-result res :attributes))))
      (cl-loop for column in data
               for column-name in column-names
               do (insert column-name ": " (format "%s" column) "\n")))))


;; This function called for semi-compatible PostgreSQL variants that only partially implement the
;; system tables that we use to query for metainformation concerning SQL tables.
(defun pgmacs--list-tables-basic ()
  "Return a list of table-names and associated metadata for current database.
Table names are schema-qualified if the schema is non-default.

This function called for PostgreSQL variants that don't provide full
compatibility."
  (let ((entries (list)))
    (dolist (table (pg-tables pgmacs--con))
      (let* ((tid (pg-escape-identifier table))
             (res (pg-exec pgmacs--con (format "SELECT COUNT(*) FROM %s" tid)))
             (row-count (cl-first (pg-result res :tuple 0)))
             (size (pgmacs--table-size-ondisk pgmacs--con table))
             (comment (or (pg-table-comment pgmacs--con table) "")))
        (push (list table row-count size "" comment) entries)))
    entries))

;; TODO: also include VIEWs
;;   SELECT * FROM information_schema.views
(defun pgmacs--list-tables-full ()
  "Return a list of table-names and associated metadata for the current database.
Table names are schema-qualified if the schema is non-default."
  (let ((entries (list)))
    (dolist (table (pg-tables pgmacs--con))
      (let* ((schema (if (pg-qualified-name-p table)
                         (pg-qualified-name-schema table)
                       "public"))
             (tname (if (pg-qualified-name-p table)
                        (pg-qualified-name-name table)
                      table))
             (sql "SELECT
                     pg_catalog.pg_total_relation_size(c.oid),
                     obj_description(c.oid, 'pg_class')
                   FROM pg_class c
                   JOIN pg_namespace n ON c.relnamespace = n.oid
                   WHERE (n.nspname, c.relname) = ($1, $2)")
             (res (pg-exec-prepared pgmacs--con sql `((,schema . "text") (,tname . "text"))))
             (tuple (pg-result res :tuple 0))
             (row-count 0)
             (size (cl-first tuple))
             ;; We could use function `pg-table-comment', but that would imply an additional SQL
             ;; query and this function is speed critical.
             (comment (cl-second tuple))
             (owner (pg-table-owner pgmacs--con table)))
        (push (list table row-count size owner (or comment "")) entries)))
    entries))

(defun pgmacs--list-tables ()
  (if (member (pgcon-server-variant pgmacs--con)
              '(cratedb cockroachdb spanner ydb questdb materialize spanner risingwave))
      (pgmacs--list-tables-basic)
    (pgmacs--list-tables-full)))

(defun pgmacs--available-table-privileges (con)
  (pcase (pgcon-server-variant con)
    ('postgresql
     (let ((privs '("SELECT" "INSERT" "UPDATE" "DELETE" "TRUNCATE" "REFERENCES" "TRIGGER")))
       (when (>= (pgcon-server-version-major con) 18)
         (push "MAINTAIN" privs))
       privs))
    ('cratedb
     (list "SELECT" "INSERT" "UPDATE" "DELETE"))
    ('cockroachdb
     '("SELECT" "INSERT" "UPDATE" "DELETE" "TRUNCATE" "REFERENCES" "TRIGGER"))
    (_ nil)))

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
  (interactive)
  (when (eq (pgcon-server-variant pgmacs--con) 'cratedb)
    (user-error "CrateDB does not support COPY TO STDOUT"))
  (when (eq (pgcon-server-variant pgmacs--con) 'materialize)
    (user-error "Materialize does not support COPY TO STDOUT"))
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
      ;; Could potentially call csv-align-mode here, but that is probably best left to the user to add to csv-mode-hook.
      (csv-mode))
    (pg-copy-to-buffer con sql buf)
    (goto-char (point-min))))

(defun pgmacs--table-to-md (&rest _ignore)
  "Dump the current PostgreSQL table in Markdown format into an Emacs buffer."
  (interactive)
  (when (eq (pgcon-server-variant pgmacs--con) 'cratedb)
    (user-error "CrateDB does not support COPY TO STDOUT"))
  (when (eq (pgcon-server-variant pgmacs--con) 'materialize)
    (user-error "Materialize does not support COPY TO STDOUT"))
  (let* ((con pgmacs--con)
         (db-buffer pgmacs--db-buffer)
         (table pgmacs--table)
         (t-id (pg-escape-identifier table))
         (t-pretty (pgmacs--display-identifier table))
         (buf (get-buffer-create (format "*PostgreSQL Markdown for %s*" t-pretty)))
         (sql (format "COPY %s TO STDOUT (HEADER TRUE, DELIMITER '|')" t-id)))
    (pop-to-buffer buf)
    (erase-buffer)
    (remove-overlays)
    (kill-all-local-variables)
    (setq-local pgmacs--db-buffer db-buffer)
    (pgmacs-transient-mode)
    (pg-copy-to-buffer con sql buf)
    (goto-char (point-min))
    ;; See https://github.com/sonic-net/SONiC/wiki/Special-Characters-and-Escaping#characters-that-need-to-be-escaped
    ;; Escape characters that might be interpreted as formatting commands by the Markdown parser
    (while (re-search-forward "\\([][`*_#+-.!{}()<>]\\)" nil t)
      (replace-match "\\\\\\1" nil nil))
    (goto-char (point-min))
    ;; Wrap every line of output in pipe characters (|) for Markdown parser to interpret it as a row
    (while (not (eq (point) (point-max)))
      (insert "|")
      (end-of-line)
      (insert "|")
      (forward-line)))
  (let* ((column-count (- (cl-count ?| (buffer-substring-no-properties 1 (line-end-position))) 1))
         (separator-line (concat (mapconcat 'identity (make-list column-count "|-") "") "|\n")))
    (goto-char (point-min))
    (forward-line)
    (insert separator-line)
    (goto-char (point-min))
    (require 'markdown-mode nil t)
    (when (fboundp 'markdown-mode)
      (markdown-mode)
      (markdown-cycle))))

;; Note that CrateDB does not support "GENERATED ALWAYS AS IDENTITY" columns, as of 2025-01. For
;; some distributed databases such as CrateDB and Materialize, we use a UUID column as a primary key
;; with a generated random UUID, instead of a BIGINT of type SERIAL or GENERATED AS IDENTITY.
;; Autoincremented columns are difficult to manage for distributed databases.
(defun pgmacs--add-primary-key (&rest _ignore)
  "Add a PRIMARY KEY to the current PostgreSQL table."
  (interactive)
  (let ((pk (pgmacs--table-primary-keys pgmacs--con pgmacs--table)))
    (when pk
      (user-error "Table %s already has a primary key %s" (pgmacs--display-identifier pgmacs--table) pk)))
  (cl-flet ((exists (name) (cl-find name (pg-columns pgmacs--con pgmacs--table) :test #'string=)))
    (let* ((colname (or (cl-find-if-not #'exists (list "id" "idpk" "idcol" "pk" "_id" "newpk"))
                        (error "Can't autogenerate a name for primary key")))
           (coltype (pcase (pgcon-server-variant pgmacs--con)
                      ('postgresql
                       (if (< (pgcon-server-version-major pgmacs--con) 12)
                           "SERIAL"
                         "BIGINT GENERATED ALWAYS AS IDENTITY"))
                      ;; See https://github.com/crate/crate/issues/11020 and
                      ;; https://github.com/crate/crate/issues/11032
                      ('cratedb "TEXT DEFAULT gen_random_text_uuid()")
                      ;; https://www.cockroachlabs.com/docs/stable/serial.html#generated-values-for-mode-sql_sequence
                      ('cockroachdb "UUID NOT NULL DEFAULT gen_random_uuid()")
                      ('questdb "UUID NOT NULL DEFAULT gen_random_uuid()")
                      (_ "SERIAL")))
           (sql (format "ALTER TABLE %s ADD COLUMN %s %s PRIMARY KEY"
                        (pg-escape-identifier pgmacs--table)
                        (pg-escape-identifier colname)
                        coltype)))
      (when (y-or-n-p (format "Really run SQL '%s'?" sql))
        (let ((res (pg-exec pgmacs--con sql)))
          (pgmacs--notify "%s" (pg-result res :status))))))
  (pgmacs--display-table pgmacs--table))

(defun pgmacs--proc-list-help (&rest _ignore)
  "Open a buffer describing keybindings in a proc-list buffer."
  (interactive)
  (let ((keymaps (current-active-maps)))
    (with-help-window "*PGmacs proc-list help*"
      (cl-labels ((shw (key msg)
                    (insert (propertize (format "%12s" key) 'face 'pgmacs-highlighted))
                    (insert (propertize " → " 'face 'pgmacs-muted))
                    (insert msg "\n"))
                  (shwf (fun msg)
                    (let* ((keyvec (where-is-internal fun keymaps t))
                           (keydesc (when keyvec (key-description keyvec))))
                      (if keydesc
                          (shw keydesc msg)
                        (display-warning 'pgmacs (format "Expected function %s to be bound in PGmacs buffer" fun))))))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (shwf 'pgmacs--proc-list-RET "Display details of the procedure at point")
          (shwf 'pgmacs--next-item "Move to next column")
          (shwf 'pgmacs--proc-list-delete "Delete the procedure at point")
          (shw "DEL" "Delete the procedure at point")
          (shw 'pgmacs--proc-list-rename "Rename the current procedure")
          (shw "<" "Move point to the first row in the table")
          (shw ">" "Move point to the last row in the table")
          (shw "{" "Shrink the horizontal space used by the current column")
          (shw "}" "Grow the horizontal space used by the current column")
          ;; (shw "r" "Redraw the table without refetching data from PostgreSQL")
          ;; (shw "g" "Redraw the table (refetches data from PostgreSQL)")
          (shwf 'pgmacs--switch-to-database-buffer "Switch to the main table-list buffer for this database")
          (shwf 'bury-buffer "Bury this buffer")
          (shrink-window-if-larger-than-buffer)
          (goto-char (point-min)))))))

;; TODO: add functionality to call a procedure with a widget-based interface for the procedure
;; arguments.
;;
;; TODO: allow user to edit the definition of a function, using a query such as that provided
;; at https://stackoverflow.com/questions/12148914/get-definition-of-function-sequence-type-etc-in-postgresql-with-sql-query
(cl-defun pgmacs--proc-list-RET (&rest _ignore)
  "Handle RET on a row in the proc-list buffer PROC-ROW."
  (interactive)
  (pgmacs--start-progress-reporter "Retrieving procedure data from PostgreSQL")
  (let* ((proc-row (pgmacstbl-current-object))
         (oid (nth 6 proc-row)))
    (unless oid
      (message "No further procedure information on this database variant")
      (cl-return-from pgmacs--proc-list-RET nil))
    (let* ((db-buffer pgmacs--db-buffer)
           (con pgmacs--con)
           (sql "SELECT pg_catalog.pg_get_function_arguments(p.oid) AS arguments,
                      t.typname AS return_type,
                      CASE WHEN l.lanname = 'internal' THEN p.prosrc
                           ELSE pg_catalog.pg_get_functiondef(p.oid)
                           END AS definition
                    FROM pg_catalog.pg_proc p
                    LEFT JOIN pg_catalog.pg_language l ON p.prolang = l.oid
                    LEFT JOIN pg_catalog.pg_type t ON t.oid = p.prorettype
                    WHERE p.oid = $1")
           (ps-name (pg-ensure-prepared-statement con "QRY-proc-details" sql (list "int4")))
           (res (pg-fetch-prepared con ps-name `((,oid . "int4"))))
           (tuple (pg-result res :tuple 0))
           (buf (get-buffer-create "*PostgreSQL procedure*")))
      (pop-to-buffer buf)
      (kill-all-local-variables)
      (setq-local pgmacs--con con
                  pgmacs--db-buffer db-buffer
                  buffer-read-only t
                  truncate-lines t)
      (pgmacs-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (remove-overlays)
        (insert (propertize "Name" 'face 'bold))
        (insert (format ": %s.%s\n" (cl-first proc-row) (cl-second proc-row)))
        (insert (propertize "Arguments" 'face 'bold))
        (insert (format ": %s\n" (cl-first tuple)))
        (insert (propertize "Return type" 'face 'bold))
        (insert (format ": %s\n" (cl-second tuple)))
        (insert (propertize "Definition" 'face 'bold))
        (insert (format ": %s\n" (cl-third tuple)))
        (goto-char (point-min))
        (shrink-window-if-larger-than-buffer)))
  (pgmacs--stop-progress-reporter)))

(cl-defun pgmacs--proc-list-rename (&rest _ignore)
  "Rename the procedure or function at point in a PGmacs proc-list buffer."
  (interactive)
  (let* ((proc-row (pgmacstbl-current-object))
         (oid (nth 6 proc-row)))
    (unless oid
      (message "Can't rename procedures in this PostgreSQL variant")
      (cl-return-from pgmacs--proc-list-rename nil))
    (let* ((con pgmacs--con)
           (schema (nth 0 proc-row))
           (orig-name (nth 1 proc-row))
           (sql "SELECT pg_catalog.pg_get_function_arguments(p.oid)
                 FROM pg_catalog.pg_proc p
                 WHERE p.oid = $1")
           (res (pg-exec-prepared con sql `((,oid . "int4"))))
           (arg-list (cl-first (pg-result res :tuple 0)))
           (new-name (read-string (format "Rename procedure %s.%s to: " schema orig-name)))
           ;; PostgreSQL does not allow us to use a prepared statement here.
           (sql (format "ALTER ROUTINE %s.%s(%s) RENAME TO %s"
                        (pg-escape-identifier schema)
                        (pg-escape-identifier orig-name)
                        arg-list
                        (pg-escape-identifier new-name)))
           (res (pg-exec con sql)))
      (pgmacs--notify "%s" (pg-result res :status))
      (pgmacs--display-procedures))))

(cl-defun pgmacs--proc-list-delete (&rest _ignore)
  "Delete the procedure or function at point in a PGmacs proc-list buffer."
  (interactive)
  (let* ((proc-row (pgmacstbl-current-object))
         (oid (nth 6 proc-row)))
    (unless oid
      (message "Can't delete procedures in this PostgreSQL variant")
      (cl-return-from pgmacs--proc-list-delete nil))
    (let* ((pgmacstbl (pgmacstbl-current-table))
           (con pgmacs--con)
           (sql "SELECT pg_catalog.pg_get_function_arguments(p.oid)
               FROM pg_catalog.pg_proc p
               WHERE p.oid = $1")
           (ps-name (pg-ensure-prepared-statement con "QRY-proc-list-get-arguments" sql (list "int4")))
           (res (pg-fetch-prepared con ps-name `((,oid . "int4"))))
           (arglist (cl-first (pg-result res :tuple 0)))
           (schema (nth 0 proc-row))
           (name (nth 1 proc-row)))
      (when (yes-or-no-p (format "Really delete PostgreSQL routine %s.%s? " schema name))
        (condition-case err
            ;; DROP ROUTINE deletes both functions and procedures
            (let* ((res (pg-exec con (format "DROP ROUTINE %s.%s(%s)" schema name arglist)))
                   (status (pg-result res :status)))
              (pgmacs--notify "%s" status)
              (pgmacstbl-remove-object pgmacstbl proc-row))
          (pg-error
           (message "Couldn't delete routine: %s" err)))))))

;; This variant uses the information schema support of the PostgreSQL variant, for variants that
;; don't properly populate the pg_catalog.pg_proc table.
(defun pgmacs--display-procedures/infschema (&rest _ignore)
  "Open a buffer displaying the FUNCTIONs and PROCEDURES defined in this database."
  (pgmacs--start-progress-reporter "Retrieving data from PostgreSQL")
  (let* ((db-buffer pgmacs--db-buffer)
         (con pgmacs--con)
         (sql "SELECT routine_schema,routine_name,routine_type,routine_body,routine_definition
               FROM information_schema.routines
               WHERE UPPER(routine_type) in ('FUNCTION', 'PROCEDURE')")
         (ps-name (pg-ensure-prepared-statement con "QRY-list-procedures-infschema" sql nil))
         (res (pg-fetch-prepared con ps-name nil))
         (pgmacstbl (make-pgmacstbl
                     :insert nil
                     :use-header-line nil
                     :columns (list
                               (make-pgmacstbl-column
                                :name (propertize "Schema" 'face 'pgmacs-table-header)
                                :width "15%"
                                :align 'left)
                               (make-pgmacstbl-column
                                :name (propertize "Name" 'face 'pgmacs-table-header)
                                :primary t
                                :width "35%"
                                :align 'left)
                               (make-pgmacstbl-column
                                :name (propertize "Signature" 'face 'pgmacs-table-header)
                                :width "40%"
                                :align 'left)
                               (make-pgmacstbl-column
                                :name (propertize "Type" 'face 'pgmacs-table-header)
                                :width 9
                                :align 'left)
                               (make-pgmacstbl-column
                                :name (propertize "Language" 'face 'pgmacs-table-header)
                                :width 8 :align 'right))
                     :row-colors pgmacs-row-colors
                     :face 'pgmacs-table-data
                     :objects (pg-result res :tuples)
                     :keymap pgmacs-proc-list-map
                     :getter (lambda (object column pgmacstbl)
                               (pcase (pgmacstbl-column pgmacstbl column)
                                 ("Schema" (pgmacs--display-table-name (cl-first object)))
                                 ("Name" (cl-second object))
                                 ;; FIXME improve
                                 ("Signature" (nth 3 object))
                                 ("Type" "Unknown")
                                 ("Language" (nth 5 object))))))
         (buf (get-buffer-create "*PGmacs procedures*")))
    (pop-to-buffer buf)
    (kill-all-local-variables)
    (setq-local pgmacs--con con
                pgmacs--db-buffer db-buffer
                buffer-read-only t
                truncate-lines t)
    (pgmacs-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (remove-overlays)
      (insert (propertize "PostgreSQL functions and procedures" 'face 'bold))
      (insert "\n\n")
      (pgmacstbl-insert pgmacstbl)))
  (goto-char (point-min))
  (pgmacs--stop-progress-reporter))

(defun pgmacs--display-procedures/risingwave (&rest _ignore)
  (pgmacs--start-progress-reporter "Retrieving data from Risingwave")
  (let* ((db-buffer pgmacs--db-buffer)
         (con pgmacs--con)
         (res (pg-exec con "SHOW FUNCTIONS"))
         (buf (get-buffer-create "*PGmacs procedures*")))
    (pop-to-buffer buf)
    (kill-all-local-variables)
    (setq-local pgmacs--con con
                pgmacs--db-buffer db-buffer
                buffer-read-only t
                truncate-lines t)
    (pgmacs-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (remove-overlays)
      (insert (propertize "Risingwave functions" 'face 'bold))
      (insert "\n\n")
      (pgmacs--show-pgresult buf res)))
  (goto-char (point-min))
  (pgmacs--stop-progress-reporter))


(defun pgmacs--display-procedures/postgresql (&rest _ignore)
  "Open a buffer displaying the FUNCTIONs and PROCEDURES defined in this database."
  (pgmacs--start-progress-reporter "Retrieving data from PostgreSQL")
  (let* ((db-buffer pgmacs--db-buffer)
         (con pgmacs--con)
         (sql "SELECT n.nspname AS schema_name,
                      p.proname AS specific_name,
                      CASE p.prokind
                           when 'f' then 'FUNCTION'
                           when 'p' then 'PROCEDURE'
                           when 'a' then 'AGGREGATE'
                           when 'w' then 'WINDOW'
                      END AS kind,
                      l.lanname as language,
                      pg_catalog.pg_get_function_arguments(p.oid) AS arguments,
                      t.typname as return_type,
                      p.oid
               FROM pg_catalog.pg_proc p
               LEFT JOIN pg_catalog.pg_namespace n ON p.pronamespace = n.oid
               LEFT JOIN pg_catalog.pg_language l ON p.prolang = l.oid
               LEFT JOIN pg_catalog.pg_type t ON t.oid = p.prorettype
               WHERE n.nspname NOT IN ('pg_catalog', 'information_schema')
               ORDER BY schema_name, specific_name")
         (ps-name (pg-ensure-prepared-statement con "QRY-list-procedures" sql nil))
         (res (pg-fetch-prepared con ps-name nil))
         (pgmacstbl (make-pgmacstbl
                     :insert nil
                     :use-header-line nil
                     :columns (list
                               (make-pgmacstbl-column
                                :name (propertize "Schema" 'face 'pgmacs-table-header)
                                :width "15%"
                                :align 'left)
                               (make-pgmacstbl-column
                                :name (propertize "Name" 'face 'pgmacs-table-header)
                                :primary t
                                :width "35%"
                                :align 'left)
                               (make-pgmacstbl-column
                                :name (propertize "Signature" 'face 'pgmacs-table-header)
                                :width "40%"
                                :align 'left)
                               (make-pgmacstbl-column
                                :name (propertize "Type" 'face 'pgmacs-table-header)
                                :width 9
                                :align 'left)
                               (make-pgmacstbl-column
                                :name (propertize "Language" 'face 'pgmacs-table-header)
                                :width 8 :align 'right))
                     :row-colors pgmacs-row-colors
                     :face 'pgmacs-table-data
                     :objects (pg-result res :tuples)
                     :keymap pgmacs-proc-list-map
                     :getter (lambda (object column pgmacstbl)
                               (pcase (pgmacstbl-column pgmacstbl column)
                                 ("Schema" (pgmacs--display-table-name (cl-first object)))
                                 ("Name" (cl-second object))
                                 ("Signature" (format "%s → %s" (nth 4 object) (nth 5 object)))
                                 ("Type" (nth 2 object))
                                 ("Language" (nth 3 object))))))
         (buf (get-buffer-create "*PGmacs procedures*")))
    (pop-to-buffer buf)
    (kill-all-local-variables)
    (setq-local pgmacs--con con
                pgmacs--db-buffer db-buffer
                buffer-read-only t
                truncate-lines t)
    (pgmacs-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (remove-overlays)
      (insert (propertize "PostgreSQL functions and procedures" 'face 'bold))
      (insert "\n\n")
      (pgmacstbl-insert pgmacstbl)))
  (goto-char (point-min))
  (pgmacs--stop-progress-reporter))

(defun pgmacs--display-procedures (&rest args)
  "Open a buffer displaying the FUNCTIONs and PROCEDURES defined in this database."
  (interactive)
  (pcase (pgcon-server-variant pgmacs--con)
    ((or 'postgresql 'cockroachdb 'yugabyte)
     (pgmacs--display-procedures/postgresql args))
    ('risingwave
     (pgmacs--display-procedures/risingwave args))
    (_ (pgmacs--display-procedures/infschema args))))


(defun pgmacs--display-database-list (&rest _ignore)
  "Display the list of databases visible over our PostgreSQL connection."
  (interactive)
  (let* ((db-buffer pgmacs--db-buffer)
         (con pgmacs--con)
         (sql "SELECT d.datname FROM pg_catalog.pg_database d ORDER BY 1")
         (res (pg-exec con sql)))
    (let ((buf (get-buffer-create "*PostgreSQL databases*")))
      (pop-to-buffer buf)
      (kill-all-local-variables)
      (setq-local pgmacs--con con
                  pgmacs--db-buffer db-buffer
                  buffer-read-only t
                  truncate-lines t)
      (pgmacs-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (remove-overlays)
        (insert (propertize "Databases visible to the current PostgreSQL user" 'face 'bold))
        (insert "\n\n")
        (pgmacs--show-pgresult buf res)))))

;; TODO: for QuestDB we could use the custom query "SELECT * FROM query_activity()"
(defun pgmacs--display-running-queries (&rest _ignore)
  "Display the list of queries running in PostgreSQL.
Opens a dedicated buffer if the query list is not empty."
  (interactive)
  (pgmacs--start-progress-reporter "Retrieving data from PostgreSQL")
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
             (kill-all-local-variables)
             (setq-local pgmacs--con con
                         pgmacs--db-buffer db-buffer
                         buffer-read-only t
                         truncate-lines t)
             (pgmacs-mode)
             (let ((inhibit-read-only t))
               (erase-buffer)
               (remove-overlays)
               (insert (propertize "Queries running in this PostgreSQL backend" 'face 'bold))
               (insert "\n\n")
               (pgmacs--show-pgresult buf res)
               (goto-char (point-min)))))))
  (pgmacs--stop-progress-reporter))


(defun pgmacs--display-replication-stats (&rest _ignore)
  "Display the replication status of this PostgreSQL instance."
  (interactive)
  (let* ((db-buffer pgmacs--db-buffer)
         (con pgmacs--con)
         (res (pg-exec con "SELECT * FROM pg_stat_replication"))
         (tuples (pg-result res :tuples)))
    (cond ((null tuples)
           (pgmacs--notify "Empty replication stats" nil))
          (t
           (let ((buf (get-buffer-create "*PostgreSQL replication stats*")))
             (pop-to-buffer buf)
             (kill-all-local-variables)
             (setq-local pgmacs--con con
                         pgmacs--db-buffer db-buffer
                         buffer-read-only t
                         truncate-lines t)
             (pgmacs-mode)
             (let ((inhibit-read-only t))
               (erase-buffer)
               (remove-overlays)
               (insert (propertize "Replication statistics from pg_stat_replication view" 'face 'bold))
               (insert "\n\n")
               (pgmacs--show-pgresult buf res)))))))

(defun pgmacs--run-analyze (&rest _ignore)
  "Run ANALYZE on the current PostgreSQL table."
  (interactive)
  (let* ((sql (format "ANALYZE %s" (pg-escape-identifier pgmacs--table)))
         (res (pg-exec pgmacs--con sql)))
    (pgmacs--notify "%s" (pg-result res :status))))

(defun pgmacs--run-count (&rest _ignore)
  "Count the number of rows in the current PostgreSQL table."
  (interactive)
  (pgmacs--start-progress-reporter "Retrieving data from PostgreSQL")
  (let* ((sql (format "SELECT COUNT(*) FROM %s" (pg-escape-identifier pgmacs--table)))
         (res (pg-exec pgmacs--con sql))
         (count (cl-first (pg-result res :tuple 0))))
    (pgmacs--stop-progress-reporter)
    (pgmacs--notify "Table %s has %s row%s"
                    pgmacs--table
                    count
                    (if (= count 1) "" "s"))))

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


;; This function is called both from pgmacs-read-sql (bound to "e") and from
;; pgmacs--add-where-filter.
;;
;; TODO: can we font-lock with sql-mode-postgres-font-lock-keywords ?
(defun pgmacs--read-sql-minibuffer (prompt completions history-variable)
  ;; See function read--expression in simple.el
  (minibuffer-with-setup-hook
      (lambda ()
        (set-syntax-table sql-mode-syntax-table)
        ;; cf. minibuffer-local-completion-map
        (local-set-key (kbd "<tab>") #'completion-at-point)
        (setq-local pgmacs--completions completions)
        (add-hook 'completion-at-point-functions
                  #'pgmacs--completion-at-point nil t))
    (let ((completion-styles '(basic flex)))
      (read-from-minibuffer prompt nil nil nil history-variable))))

;; The list of column names in the current table. Note that this function needs to be called in the
;; PGmacs buffer (where the text properties it consults are set), and not in the minibuffer. If we
;; implement fuzzy matching, we should escape the column names with pg-escape-identifiers, because a
;; case-sensitive SQL identifier will need quoting to be recognized by PostgreSQL.
(defun pgmacs--completion-table ()
  (save-excursion
    (pgmacstbl-beginning-of-table)
    (let* ((tbl (pgmacstbl-current-table))
           (cols (and tbl (pgmacstbl-columns tbl))))
      (append (mapcar #'pgmacstbl-column-name cols)
              pgmacs--postgresql-keywords))))

(defvar pgmacs--where-filter-history nil)

;; Bound to "W" in a row-list buffer.
(defun pgmacs--add-where-filter (&rest _ignore)
  "Add an SQL WHERE filter to the current PGmacs row-list buffer."
  (interactive)
  (unless (zerop pgmacs--offset)
    (message "Resetting table OFFSET")
    (sit-for 0.5))
  (setq pgmacs--offset 0)
  (let ((filter (pgmacs--read-sql-minibuffer "WHERE: "
                                             (pgmacs--completion-table)
                                             'pgmacs--where-filter-history)))
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
  ;; FIXME perhaps should use with-help-window
  (let ((keymaps (current-active-maps)))
    (pop-to-buffer "*PGmacs row-list help*")
    (buffer-disable-undo)
    (help-mode)
    (cl-labels ((shw (key msg)
                  (insert (propertize (format "%12s" key) 'face 'pgmacs-highlighted))
                  (insert (propertize " → " 'face 'pgmacs-muted))
                  (insert msg "\n"))
                (shwf (fun msg)
                  (let* ((keyvec (where-is-internal fun keymaps t))
                         (keydesc (when keyvec (key-description keyvec))))
                    (if keydesc
                        (shw keydesc msg)
                      (display-warning 'pgmacs (format "Expected function %s to be bound in PGmacs buffer" fun))))))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (shwf 'pgmacs--view-value "Display the value at point in a dedicated buffer")
        (shwf 'pgmacs--row-list-dwim "Edit the value at point in the minibuffer")
        (shwf 'pgmacs--next-item "Move to next column")
        (shwf 'pgmacs--edit-value-widget "Edit the value at point in a widget-based buffer")
        (shwf 'pgmacs--add-where-filter "Specify a WHERE filter to apply to displayed rows")
        (shwf 'pgmacs--row-list-delete-row "Delete the row at point")
        (shw "DEL" "Delete the row at point")
        (shwf 'pgmacs--insert-row "Insert a new row, prompting for new values in minibuffer")
        (shwf 'pgmacs--insert-row-widget "Insert a new row, prompting for new values in a dedicated buffer")
        (shwf 'pgmacs--copy-row "Copy the row at point")
        (shwf 'pgmacs--yank-row "Yank the previously copied row and insert into the table")
        (shwf 'pgmacs--row-as-json "Copy the current row to the kill-ring in JSON format")
        (shwf 'pgmacs--row-list-mark-row "Mark the current row for deletion")
        (shwf 'pgmacs--row-list-unmark-row "Unmark the current row (deselect for deletion)")
        (shwf 'pgmacs--row-list-unmark-all "Unmark all rows (deselect all for deletion)")
        (shwf 'pgmacs--row-list-delete-marked "Delete marked rows")
        (shwf 'pgmacs--row-list-rename-column "Rename the current column")
        (shwf 'pgmacs--shell-command-on-value "Run a filter-like shell command with current cell value as input")
        (shwf 'pgmacs--async-command-on-value "Run a program with the value of current cell as first argument")
        (shwf 'pgmacs--upcase-value "Upcase the value of the current cell")
        (shwf 'pgmacs--downcase-value "Downcase the value of the current cell")
        (shwf 'pgmacs--capitalize-value "Capitalize the value of the current cell")
        (shwf 'pgmacs--paginated-next "Next page of output (if table contents are paginated)")
        (shwf 'pgmacs--paginated-prev "Previous page of output (if table contents are paginated)")
        (shwf 'pgmacs-run-sql "New buffer with output from SQL query")
        (shwf 'pgmacs-run-buffer-sql "Run SQL from a buffer and display the output")
        (shw "<number>" "Move point to nth column")
        (shwf 'pgmacs--schemaspy-table "Run SchemaSpy on the current table and display the SVG output")
        (shw "<" "Move point to the first row in the table")
        (shw ">" "Move point to the last row in the table")
        (shw "{" "Shrink the horizontal space used by the current column")
        (shw "}" "Grow the horizontal space used by the current column")
        (shwf 'pgmacs-open-table "Prompt for a table name and open a new buffer displaying that table's data")
        (shwf 'pgmacs--redraw-pgmacstbl "Redraw the table without refetching data from PostgreSQL")
        (shwf 'pgmacs--row-list-redraw "Redraw the table (refetches data from PostgreSQL)")
        (shwf 'pgmacs--switch-to-database-buffer "Switch to the main table-list buffer for this database")
        (shwf 'bury-buffer "Bury this buffer")
        (shrink-window-if-larger-than-buffer)
        (goto-char (point-min))))))

;; Bound to "d" in a row-list buffer.
;;
;; How to add a face attribute (such as :background "red") to the entire line? It won't work to look
;; at the face at one point in the line and simply add (or replace) a component to that, because
;; some columns are using special face features such as bold (primary key) and blue foreground
;; (foreign key references). So we need to be "adding" an attribute, using add-face-text-property.
;; But deleting that later is tricky...
(cl-defun pgmacs--row-list-mark-row (&rest _ignore)
  "Mark the current row for deletion."
  (interactive)
  (when (null pgmacs--table-primary-keys)
    (message "Can't delete from a table that has no PRIMARY KEY")
    (cl-return-from pgmacs--row-list-mark-row))
  ;; Note: this line number is zero-based
  (when-let* ((line-number (get-text-property (point) 'pgmacstbl-line-number)))
    (cl-pushnew line-number pgmacs--marked-rows)
    (let* ((table (pgmacstbl-current-table))
           (buffer-read-only nil))
      (pgmacstbl-mark-row table line-number :marked-for-deletion)
      (add-face-text-property (pos-bol) (pos-eol) `(:background ,pgmacs-deleted-color))
      ;; Move point to next row, unless we are already on the last row.
      (let ((pos (point)))
        (forward-line 1)
        (when (eobp)
          (goto-char pos))))))

;; Bound to "u" in a row-list buffer.
(cl-defun pgmacs--row-list-unmark-row (&rest _ignore)
  "Unmark the current row for deletion."
  ;; Note: this line number is zero-based
  (interactive)
  (when-let* ((line-number (get-text-property (point) 'pgmacstbl-line-number)))
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
  (interactive)
  (let* ((table (pgmacstbl-current-table))
         (buffer-read-only nil))
    (dolist (line-number pgmacs--marked-rows)
      (pgmacstbl-unmark-row table line-number))
    (setq pgmacs--marked-rows (list)))
  (pgmacs--redraw-pgmacstbl))

;; Bound to "x" in a row-list buffer.
(cl-defun pgmacs--row-list-delete-marked (&rest _ignore)
  "Delete rows in the current table marked for deletion using `\\[pgmacs--row-list-mark-row]'.
Deletion is only possible for tables with a (possibly multicolumn) primary key."
  (interactive)
  (when (null pgmacs--marked-rows)
    (message "No rows are marked for deletion")
    (cl-return-from pgmacs--row-list-delete-marked))
  (when (null pgmacs--table-primary-keys)
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
          (dolist (pk pgmacs--table-primary-keys)
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
        (pgmacs-flush-table pgmacs--con pgmacs--table)
        (pgmacs--notify "%s" status)
        (unless (string-prefix-p "DELETE " status)
          (error "Unexpected status %s for PostgreSQL DELETE command" status))
        (let ((rows-affected (cl-parse-integer (substring status 7))))
          (cond ((eql 0 rows-affected)
                 (display-warning 'pgmacs "Could not delete PostgreSQL rows")
                 (pg-exec pgmacs--con "COMMIT TRANSACTION"))
                ((eql (length pgmacs--marked-rows) rows-affected)
                 (pg-exec pgmacs--con "COMMIT TRANSACTION")
                 (dolist (line-number pgmacs--marked-rows)
                   (let ((row (nth line-number (pgmacstbl-objects pgmacstbl))))
                     (pgmacstbl-remove-object pgmacstbl row)))
                 (pgmacs--redraw-pgmacstbl))
                (t
                 (display-warning 'pgmacs "Deletion affected more than 1 row; rolling back")
                 (pg-exec pgmacs--con "ROLLBACK TRANSACTION"))))))
    (setq pgmacs--marked-rows (list))))

;; FIXME this SQL query does not work with YDB (BAD_REQUEST message) nor with RisingWave (unimplemented).
(defun pgmacs--row-list-rename-column (&rest _ignore)
  "Rename the current PostgreSQL column."
  (interactive)
  (let* ((col-id (pgmacstbl-current-column))
         (cols (pgmacstbl-columns (pgmacstbl-current-table)))
         (col (nth col-id cols))
         (col-name (pgmacstbl-column-name col))
         (new (read-string (format "Rename column %s to: " col-name)))
         (sql (format "ALTER TABLE %s RENAME COLUMN %s TO %s"
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
  (pcase (pgcon-server-variant con)
    ;; YDB (as of 2025-03) triggers an error when using OFFSET without using LIMIT...
    ('ydb
     (let ((sql (format "SELECT * FROM %s LIMIT %s OFFSET %s"
                        table-name-escaped row-count offset)))
       (pg-exec con sql)))
    ;; QuestDB does not support OFFSET.
    ;; https://questdb.com/docs/reference/sql/limit/
    ('questdb
     (let ((sql (format "SELECT * FROM %s LIMIT %s,%s"
                        table-name-escaped offset (+ offset row-count))))
       (pg-exec con sql)))
    (_
     (let ((sql (format "SELECT * FROM %s OFFSET %s" table-name-escaped offset)))
       (pg-exec-prepared con sql (list) :max-rows row-count)))))

(defun pgmacs--select-rows-where (con table-name-escaped where-filter row-count)
  (when (cl-search ";" where-filter)
    (user-error "WHERE filter must not contain end-of-statement marker ';'"))
  (let ((case-fold-search t))
    (when (cl-search "WHERE" where-filter :test #'char-equal)
      (user-error "WHERE filter should be specified without the WHERE keyword")))
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
rows to display in the table. The WHERE clause does not include the
SQL keyword WHERE (example: `column_name > 0').

The CENTER-ON and WHERE-FILTER arguments are mutually exclusive.

Runs functions on `pgmacs-row-list-hook'."
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
           (header1 (format "PostgreSQL table %s" t-pretty))
           ;; Will be null for some semi-compatible PostgreSQL variants
           (header2 (if owner (format ", owned by %s" owner-displayed) "")))
      (erase-buffer)
      (insert
       (propertize header1 'face 'bold)
       (propertize header2 'face 'bold)
       "\n"))
    ;; This is a buffer-local variable.
    (setq pgmacs--table-primary-keys (pgmacs--table-primary-keys con table))
    (let* ((comment (pg-table-comment con table))
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
                       :keymap pgmacs-row-list-map/table)))
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
        (insert-text-button (propertize "Modify"  'font-lock-face '(:box t))
                            'action (lambda (&rest _ignore)
                                      (let ((comment (read-from-minibuffer "New table comment: ")))
                                        (setf (pg-table-comment con table) comment))
                                      (pgmacs--display-table table))
                            'help-echo "Modify the table comment")
        (insert "\n"))
      (let* ((size/disk (pgmacs--table-size-ondisk con table))
             (size/pretty (and size/disk (file-size-human-readable size/disk 'iec " ")))
             (idx/disk (pgmacs--index-size-ondisk con table))
             (idx/pretty (and idx/disk (file-size-human-readable idx/disk 'iec " "))))
        (when size/pretty
          (insert (propertize "On-disk-size" 'face 'bold))
          (insert ": " size/pretty)
          (when idx/pretty
            (insert (format " (indexes %s)" idx/pretty)))
          (insert "\n")))
      (insert (propertize "Columns" 'face 'bold) ":\n")
      (cl-loop
       for col in column-names
       for col-count from 1
       do (let* ((structure-char (if (eql col-count (length column-names))
                                     (if (char-displayable-p ?└) ?└ ?`)
                                   (if (char-displayable-p ?├) ?├ ?|)))
                 (ci (gethash col column-info))
                 (cif (pgmacs--format-column-info ci)))
            (insert (propertize (format "%c " structure-char) 'face 'shadow))
            (insert (format "%s: %s  " col cif))
            (unless (member (pgcon-server-variant con) '(risingwave))
              (insert-text-button "Rename column"
                                  'action `(lambda (&rest _ignore)
                                             (let* ((prompt (format "New name for column %s: " ,col))
                                                    (new (read-from-minibuffer prompt))
                                                    (sql (format "ALTER TABLE %s RENAME COLUMN %s TO %s"
                                                                 (pg-escape-identifier ,table)
                                                                 (pg-escape-identifier ,col)
                                                                 (pg-escape-identifier new)))
                                                    (res (pg-exec ,con sql)))
                                               (pgmacs--notify "%s" (pg-result res :status))
                                               (pgmacs--display-table ,table)))
                                  'help-echo "Rename this column"))
            (insert "   ")
            (cond ((pg-column-comment con table col)
                   (insert-text-button
                    "Modify column comment"
                    'action `(lambda (&rest _ignore)
                               (let* ((prompt (format "New column comment for %s: " ,col))
                                      (new (read-from-minibuffer prompt)))
                                 (setf (pg-column-comment ,con ,table ,col) new)
                                 (pgmacs--notify "Column comment updated")
                                 (pgmacs--display-table ,table))))
                   (insert "   ")
                   (insert-text-button
                    "Delete column comment"
                    'action `(lambda (&rest _ignore)
                               (when (y-or-n-p (format "Really delete comment on column %s?" ,col))
                                 (setf (pg-column-comment ,con ,table ,col) nil)
                                 (pgmacs--notify "Column comment deleted")
                                 (pgmacs--display-table ,table)))
                    'help-echo "Delete comment on column"))
                  ;; And if there is no existing column comment
                  (t
                   ;; FIXME this functionality is not implemented for YDB
                   (insert-text-button
                    (format "%sAdd column comment" (or (pgmacs--maybe-svg-icon #'pgmacs--svg-icon-comment) ""))
                    'action `(lambda (&rest _ignore)
                               (let* ((prompt (format "New comment for column %s: " ,col))
                                      (new (read-from-minibuffer prompt)))
                                 (setf (pg-column-comment ,con ,table ,col) new)
                                 (pgmacs--notify "Column comment updated")
                                 (pgmacs--display-table ,table))))))
            (insert "\n")))
      (let ((fkc (pgmacs--fk-constraints con table)))
        (when fkc
          (insert (propertize "Foreign key constraints" 'face 'bold) ":\n")
          (dolist (c fkc)
            (insert "  " (cl-first c) " " (cl-second c) "\n"))))
      (when indexes
        (insert (propertize "Indexes" 'face 'bold) ":\n")
        (dolist (idx indexes)
          (cl-multiple-value-bind (name unique-p primary-p clustered-p valid-p _def type cols) idx
              (insert (format "  %s %s%s%s%s %s (cols: %s) "
                              name
                              (if unique-p "UNIQUE " "")
                              (if primary-p "PRIMARY " "")
                              (if clustered-p "CLUSTERED " "")
                              (if valid-p "" "INVALID ")
                              type cols))
              (insert-text-button "index stats"
                                  'action (lambda (&rest _ignore) (pgmacs--show-index-stats con name))
                                  'help-echo "Display pgstatindex data")
              (insert "\n"))))
      ;; has_table_privilege ( [ user name or oid, ] table text or oid, privilege text ) → boolean
      (when (pg-function-p con "has_table_privilege")
        (let ((items (list)))
          (dolist (priv (pgmacs--available-table-privileges con))
            (let* ((res (pg-exec-prepared con "SELECT has_table_privilege($1, $2)" `((,t-id . "text") (,priv . "text"))))
                   (tuple (pg-result res :tuple 0))
                   (color (if (cl-first tuple) "green" "red")))
              (push (pgmacs--make-badge priv :color color) items)))
          (when items
            (insert "Table privileges for current user: " (string-join (reverse items) " ") "\n"))))
      (when-let* ((acl (pg-table-acl con table)))
        (insert (format "Table ACL: %s" acl) "\n"))
      (insert "Row-level access control: ")
      (if (pgmacs--row-security-active con table)
          (insert "enabled")
        (insert "not enabled"))
      (insert "\n\n")
      (dolist (btn pgmacs-row-list-buttons)
        (when (pgmacs--insert btn)
          (insert "  ")
          (when (> (current-column) (min (window-width) 90))
            (insert "\n"))))
      (unless (bolp)
        (insert "\n"))
      ;; Make it visually clear to the user that a WHERE filter is active
      (when where-filter
        (insert (propertize "WHERE filter" 'face 'bold) ": ")
        (insert (propertize where-filter 'face 'pgmacs-where-filter) "\n\n"))
      (when pgmacs--offset
        (pgmacs-paginated-mode)
        (when (>= pgmacs--offset pgmacs-row-limit)
          (insert-text-button
           (format "Prev. %s rows" pgmacs-row-limit)
           'action #'pgmacs--paginated-prev
           'help-echo "Move to previous page of results")
          (insert "   "))
        (when (pg-result res :incomplete)
          (insert-text-button
           (format "Next %s rows" pgmacs-row-limit)
           'action #'pgmacs--paginated-next
           'help-echo "Move to next page of results"))
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
              (add-face-text-property (pos-bol) (pos-eol) `(:background ,pgmacs-deleted-color))))))
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
           finally do (message "Didn't find row matching %s" pk-val)))))
    (run-hooks 'pgmacs-row-list-hook)))

(defun pgmacs--row-list-redraw (&rest _ignore)
  "Refresh a PostgreSQL row-list buffer.
This refetches data from PostgreSQL and runs hooks on `pgmacs-row-list-hook'."
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
      (setq pgmacs--offset offset))
    (run-hooks 'pgmacs-row-list-hook)))

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
  (interactive)
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


;; Used in row-list buffer: if point is on a column which REFERENCES a foreign table, then jump to
;; that table on the appropriate row; otherwise prompt to edit using pgmacs--edit-value-minibuffer
(defun pgmacs--row-list-dwim (&rest _ignore)
  (interactive)
  (let* ((colinfo (get-text-property (point) 'pgmacs--column-info))
         (refs (and colinfo (gethash "REFERENCES" colinfo)))
         (current-row (pgmacstbl-current-object)))
    (if refs
        (let* ((table (cl-first refs))
               (pk (cl-second refs))
               (pk-col-id (pgmacstbl-current-column))
               (pk-col-type (aref pgmacs--column-type-names pk-col-id))
               (pk-val (nth pk-col-id current-row))
               (center-on (and pk (list pk pk-val pk-col-type))))
          (pgmacs--display-table table :center-on center-on))
      (pgmacs--edit-value-minibuffer current-row))))

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

;; This is similar to pgmacstbl-revert, but works correctly with a buffer that contains content other
;; than the pgmacstbl.
(defun pgmacs--redraw-pgmacstbl (&rest _ignore)
  "Redraw the pgmacstbl in the current buffer."
  (interactive)
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

(defun pgmacs--read-current-user (con)
  (let* ((res (pg-exec con "SELECT current_user"))
         (row (pg-result res :tuple 0)))
    (cl-first row)))

(defun pgmacs--read-current-setting (con setting)
  (pcase (pgcon-server-variant con)
    ;; RisingWave does not implement the two-argument version of current_setting()
    ('risingwave
     (ignore-errors
       (let* ((res (pg-exec-prepared con "SELECT pg_catalog.current_setting($1)"
                                     `((,setting . "text"))))
              (tuples (pg-result res :tuples)))
         (caar tuples))))
    ;; YDB does not allow access to the current_setting function
    ('ydb nil)
    (_
     ;; The second argument of true means don't raise an error if the setting is not defined.
     (let* ((res (pg-exec-prepared con "SELECT pg_catalog.current_setting($1, true)"
                                   `((,setting . "text"))))
            (tuples (pg-result res :tuples)))
       (caar tuples)))))

(defun pgmacs--display-backend-information (&rest _ignore)
  "Create a buffer with information concerning the current PostgreSQL backend."
  (interactive)
  (let ((con pgmacs--con)
        (db-buffer pgmacs--db-buffer)
        (inhibit-read-only t))
    (cl-flet ((show (setting label)
                (let ((value (pgmacs--read-current-setting con setting)))
                  (when value
                    (insert (format "%s: %s\n" label value))))))
      (pop-to-buffer (get-buffer-create "*PostgreSQL backend information*"))
      (setq buffer-read-only nil)
      (erase-buffer)
      (remove-overlays)
      (kill-all-local-variables)
      (setq-local pgmacs--con con
                  pgmacs--db-buffer db-buffer
                  buffer-read-only nil)
      (buffer-disable-undo)
      (unless (member (pgcon-server-variant con) '(cratedb questdb ydb spanner materialize risingwave))
        (let* ((res (pg-exec con "SELECT inet_server_addr(), inet_server_port(), pg_backend_pid()"))
               (row (pg-result res :tuple 0))
               (addr (cl-first row))
               (port (cl-second row))
               (pid (cl-third row)))
          (insert (format "Connected to backend with pid %s" pid))
          (if addr
              (insert (format " at %s:%s\n" addr port))
            (insert " over Unix-domain socket\n"))))
      (let ((variant (pgcon-server-variant con)))
        (unless (eq variant 'postgresql)
          (insert (format "Server appears to be the PostgreSQL variant %s\n" variant))))
      (unless (member (pgcon-server-variant con) '(cockroachdb cratedb yugabyte ydb xata greptimedb risingwave))
        (when (> (pgcon-server-version-major con) 11)
          (let* ((res (pg-exec con "SELECT pg_catalog.current_setting('ssl_library')"))
                 (row (pg-result res :tuple 0)))
            (insert (format "Backend compiled with SSL library %s\n" (cl-first row)))))
        (let* ((res (pg-exec con "SELECT pg_catalog.current_setting('data_checksums')"))
               (row (pg-result res :tuple 0)))
          (insert (format "Data checksums: %s\n" (cl-first row)))))
      (let* ((superuser-p (pgmacs--read-current-setting con "is_superuser")))
        (insert (format "Connected as user %s (%ssuperuser)\n"
                        (pgmacs--read-current-user con)
                        (if superuser-p "" "not "))))
      (show "in_hot_standby" "In hot standby")
      (unless (member (pgcon-server-variant con) '(cockroachdb))
        (let* ((res (pg-exec con "SELECT pg_catalog.pg_postmaster_start_time()"))
               (dtime (car (pg-result res :tuple 0)))
               (fmt (funcall (pgmacs--value-formatter "timestamp") dtime)))
          (insert (format "PostgreSQL running since %s\n" fmt))))
      (show "client_encoding" "Client encoding")
      (show "server_encoding" "Server encoding")
      (show "TimeZone" "Server timezone")
      (show "shared_memory_size" "Server shared memory size")
      (show "datestyle" "Date style")
      (show "search_path" "Search path")
      (unless (member (pgcon-server-variant con) '(cockroachdb cratedb materialize risingwave))
        (let* ((res (pg-exec con "SELECT pg_catalog.pg_listening_channels()"))
               (channels (pg-result res :tuples)))
          (when channels
            (insert "Asynchronous notification channels for the current session:\n")
            (dolist (ch channels)
              (insert "  " ch "\n")))))
      (unless (member (pgcon-server-variant con) '(cratedb materialize risingwave))
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
                              (message "Loading extension %s failed: %s" (car ext) err))))
                         ;; Refresh the buffer and move to the relevant line showing the extension
                         (pgmacs--display-backend-information)
                         (search-forward (car ext))
                         (recenter))))
            (insert "\n"))))
      (pcase (pgcon-server-variant con)
        ('cockroachdb
         (insert "\nCockroachDB users\n")
         (let ((res (pg-exec con "SHOW USERS")))
           (pgmacs--show-pgresult (current-buffer) res)))
        ('risingwave
         (insert "\nRisingwave 'show parameters' output\n")
         (let ((res (pg-exec con "SHOW PARAMETERS")))
           (pgmacs--show-pgresult (current-buffer) res)))
        ('materialize
         (insert "\nMaterialize 'show all' output\n")
         (let ((res (pg-exec con "SHOW ALL")))
           (pgmacs--show-pgresult (current-buffer) res)))
        ('cratedb
         (let* ((res (pg-exec con "SELECT name, settings FROM sys.cluster"))
                (row (pg-result res :tuple 0)))
           ;; json-pretty-print-buffer
           (insert "\nCrateDB cluster name: " (cl-first row) "\n")
           (when (json-available-p)
             (insert "CrateDB settings: ")
             (insert (with-temp-buffer
                       (json-insert (cl-second row))
                       (json-pretty-print-buffer)
                       (buffer-string)))
             (insert "\n")))))
      (shrink-window-if-larger-than-buffer)
      (goto-char (point-min))
      (pgmacs-transient-mode)
      (setq buffer-read-only t))))


(defvar pgmacs--stat-activity-columns
  (list "datname" "usename" "client_addr" "backend_start" "xact_start" "query_start" "wait_event"))

(defun pgmacs--display-stat-activity (&rest _ignore)
  "Display information from PostgreSQL's pg_stat_activity table."
  (interactive)
  (let* ((cols (string-join pgmacs--stat-activity-columns ","))
         (sql (format "SELECT %s FROM pg_catalog.pg_stat_activity" cols)))
    (pgmacs-show-result pgmacs--con sql)))

(defun pgmacs--display-jobs/risingwave (&rest _ignore)
  "Display information on Risingwave streaming jobs in progress."
  (interactive)
  (pgmacs-show-result pgmacs--con "SHOW JOBS"))

(defun pgmacs--display-connections/materialize (&rest _ignore)
  "Display information on Materialize connections."
  (interactive)
  (pgmacs-show-result pgmacs--con "SHOW CONNECTIONS"))

(defvar pgmacs--run-sql-history nil)

(defun pgmacs-run-sql (&rest _ignore)
  "Prompt for an SQL query and display the output in a dedicated buffer."
  (interactive)
  (let* ((completions (append (pg-tables pgmacs--con)
                              pgmacs--postgresql-keywords))
         (completion-ignore-case t)
         (sql (pgmacs--read-sql-minibuffer "SQL query: "
                                            completions
                                            'pgmacs--run-sql-history)))
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
    (erase-buffer)
    (remove-overlays)
    (insert (propertize "PostgreSQL query output" 'face 'bold))
    (insert "\n")
    (insert (propertize "SQL" 'face 'bold))
    (insert (format ": %s\n\n" sql)))
  (let* ((res (pg-exec con sql)))
    (pgmacs--show-pgresult (current-buffer) res))
  (shrink-window-if-larger-than-buffer)
  (pgmacs--stop-progress-reporter))

(defun pgmacs--show-pgresult (buffer pgresult)
  (with-current-buffer buffer
    (let ((rows (pg-result pgresult :tuples))
          (attributes (pg-result pgresult :attributes))
          (con pgmacs--con)
          (inhibit-read-only t))
      (cond ((null rows)
             (insert (format "Query status: %s\n\n" (pg-result pgresult :status)))
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
                    ;; FIXME we should be using a keymap without the destructive operations here
                    (pgmacstbl (make-pgmacstbl
                                :insert nil
                                :use-header-line nil
                                :face 'pgmacs-table-data
                                :columns columns
                                :row-colors pgmacs-row-colors
                                :objects rows
                                :keymap pgmacs-row-list-map/table)))
               (pgmacstbl-insert pgmacstbl)
               (goto-char (point-min))))))))

;; Bound to TAB in table-list, row-list, proc-list buffers.
(defun pgmacs--next-item (&rest _ignore)
  "Move to the next column or next active button in a PGmacs buffer."
  (interactive)
  (let ((tbl (pgmacstbl-current-table)))
    (if tbl
        ;; If we are inside the data table, move to the next column (including moving down a line if
        ;; we are on the last column).
        (let* ((column-count (length (pgmacstbl-columns tbl)))
               (current-col (pgmacstbl-current-column)))
          (when current-col
            (cond ((eql current-col (1- column-count))
                   (forward-line)
                   (pgmacstbl-goto-column 0))
                  (t
                   (pgmacstbl-next-column)))))
      ;; Outside the data table, move to the next active button, or to the beginning of the data table.
      (let ((btn (next-button (point))))
        (if btn
            (goto-char btn)
          (text-property-search-forward 'pgmacstbl)
          (text-property-search-backward 'pgmacstbl))))))


;; If the cursor is on the Comment column, allow the user to set the table comment. Otherwise,
;; display the table in a separate buffer.
(defun pgmacs--table-list-RET (&rest _ignore)
  "In the table-list buffer, open the table at point or update table comment.
Called on RET on a line in the table-list buffer."
  (interactive)
  (let* ((tbl (pgmacstbl-current-table))
         (table-row (pgmacstbl-current-object))
         (col-id (pgmacstbl-current-column))
         (col (nth col-id (pgmacstbl-columns tbl)))
         (col-name (pgmacstbl-column-name col))
         (table (cl-first table-row)))
    (cond ((string= "Comment" col-name)
           (let ((comment (read-from-minibuffer "New table comment: "))
                 (new-row (copy-sequence table-row)))
             (setf (pg-table-comment pgmacs--con table) comment)
             (setf (nth col-id new-row) comment)
             (message "Updating table comment for %s" table)
             (pgmacstbl-update-object tbl new-row table-row)
             (pgmacs--redraw-pgmacstbl)))
          ;; TODO perhaps allow changes to table owner (if we are superuser)
          (t
           (pgmacs--display-table table)))))

(defun pgmacs--table-list-delete (&rest _ignore)
  "Delete (drop) the PostgreSQL table specified by TABLE-ROW."
  (interactive)
  (let* ((pgmacstbl (pgmacstbl-current-table))
         (table-row (pgmacstbl-current-object))
         (table (car table-row))
         (t-id (pg-escape-identifier table)))
    (when (yes-or-no-p (format "Really drop PostgreSQL table %s? " t-id))
      ;; We can't use a prepared statement for this dynamic SQL statement
      (let* ((sql (format "DROP TABLE %s" t-id))
             (res (pg-exec pgmacs--con sql)))
        (pgmacs--notify "%s" (pg-result res :status))
        (pgmacstbl-remove-object pgmacstbl table-row)
        (pgmacs--redraw-pgmacstbl)))))

(defun pgmacs--table-list-rename (&rest _ignore)
  "Rename the PostgreSQL table at point."
  (interactive)
  (let* ((pgmacstbl (pgmacstbl-current-table))
         (table-row (pgmacstbl-current-object))
	 (table (car table-row))
         (t-id (pg-escape-identifier table))
         (new (read-string (format "Rename table %s to: " t-id)))
         (new-id (pg-escape-identifier new)))
    (let* ((sql (format "ALTER TABLE %s RENAME TO %s" t-id new-id))
           (res (pg-exec pgmacs--con sql)))
      (pgmacs-flush-table pgmacs--con t-id)
      (pgmacs--notify "%s" (pg-result res :status))
      (let ((new-row (copy-sequence table-row)))
	(setf (cl-first new-row) new)
        (pgmacstbl-update-object pgmacstbl new-row table-row))
      ;; Redraw in the table-list buffer.
      (pgmacs--redraw-pgmacstbl))))

(defun pgmacs--table-list-redraw (&rest _ignore)
  "Refresh the PostgreSQL table-list buffer.
Runs functions on `pgmacs-table-list-hook'."
  (interactive)
  (let ((con pgmacs--con))
    (kill-buffer)
    (pgmacs-open con)))

(defun pgmacs--table-list-help (&rest _ignore)
  "Show keybindings active in a table-list buffer."
  (interactive)
  (let ((keymaps (current-active-maps)))
    (pop-to-buffer "*PGmacs table-list help*")
    (erase-buffer)
    (buffer-disable-undo)
    (help-mode)
    (cl-labels ((shw (key msg)
                  (insert (propertize (format "%12s" key) 'face '(:foreground "blue")))
                  (insert (propertize " → " 'face '(:foreground "gray")))
                  (insert msg "\n"))
                (shwf (fun msg)
                  (let* ((keyvec (where-is-internal fun keymaps t))
                         (keydesc (when keyvec (key-description keyvec))))
                    (if keydesc
                        (shw keydesc msg)
                      (display-warning 'pgmacs (format "Expected function %s to be bound in PGmacs buffer" fun))))))
      (let ((inhibit-read-only t))
        (shwf 'pgmacs--table-list-RET "Open a new buffer to browse/edit the table at point")
        (shwf 'pgmacs--next-item "Move to next column or button")
        (shwf 'pgmacs--table-list-delete "Delete the table at point")
        (shwf 'pgmacs--table-list-rename "Rename the table at point")
        (shwf 'pgmacs-open-table "Prompt for a table to browse/edit in a new buffer")
        (shwf 'pgmacs--display-procedures "New buffer listing the functions and procedures in the current database")
        (shwf 'pgmacs-run-sql "New buffer with output from SQL query")
        (shwf 'pgmacs-run-buffer-sql "Run buffer SQL and display the output")
        (shwf 'pgmacs--schemaspy-database "Run SchemaSpy on the current database and display the SVG output")
        (shw "<" "Go to the first table in the table list")
        (shw ">" "Go to the last table in the table list")
        (shw "{" "Shrink the horizontal space used by the current column")
        (shw "}" "Grow the horizontal space used by the current column")
        (shwf 'pgmacs--table-list-redraw "Redraw this table-list buffer (refetches data from PostgreSQL)")
        (shwf 'bury-buffer "Bury this buffer")
        (shrink-window-if-larger-than-buffer)
        (goto-char (point-min))))))

;; This function generates the string used to display a table in the main table-list buffer. If the
;; display is able to display SVG images, we prefix the name with a little SVG icon of a table.
(defun pgmacs--display-table-name (name)
  (let* ((name (pgmacs--display-identifier name))
         (maybe-icon (pgmacs--maybe-svg-icon #'pgmacs--svg-icon-table)))
    (concat maybe-icon name)))


;; TODO: On CrateDB we could query "SELECT ssl FROM sys.sessions WHERE id=<todo>"
(defun pgmacs--tls-status (con)
  (if (and (not (member (pgcon-server-variant con) '(cockroachdb cratedb yugabyte ydb xata greptimedb risingwave)))
           (> (pgcon-server-version-major con) 11))
      (let ((res (pg-exec con "SHOW ssl")))
        (cl-first (pg-result res :tuple 0)))
    "unknown"))

;;;###autoload
(defun pgmacs-open (con)
  "Browse the contents of PostgreSQL database to which we are connected over CON."
  (when pgmacs-enable-query-logging
    (message "Enabling PGmacs query logging")
    (pg-enable-query-log con))
  (pg-hstore-setup con)
  (pg-vector-setup con)
  (when pgmacs-use-header-line
    (setq pgmacs-header-line
          (list (when (char-displayable-p ?🐘) " 🐘")
                (propertize " PGmacs " 'face 'bold)
                ;; (list :tcp host port dbname user password)
                (let* ((ci (pgcon-connect-info con))
                       (tls (pgmacs--tls-status con))
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
    (unless (eq 'postgresql (pgcon-server-variant con))
      (insert (format "Connected to PostgreSQL variant %s\n" (pgcon-server-variant con))))
    (insert (pg-backend-version con)))
  (when (eq 'questdb (pgcon-server-variant con))
    (let* ((res (pg-exec con "SELECT build()"))
           (row (pg-result res :tuple 0))
           (inhibit-read-only t))
      (insert "\n" (cl-first row))))
  (let* ((dbname (pgcon-dbname con))
         (inhibit-read-only t)
         (pgmacstbl (make-pgmacstbl
                  :insert nil
                  :use-header-line nil
                  :columns (list
                            (make-pgmacstbl-column
                             :name (propertize "Table" 'face 'pgmacs-table-header)
                             :width pgmacs-row-list-table-name-width
                             :primary t
                             :align 'left)
                            (make-pgmacstbl-column
                             :name (propertize "Rows" 'face 'pgmacs-table-header)
                             :width 7 :align 'right)
                            (make-pgmacstbl-column
                             :name (propertize "Size on disk" 'face 'pgmacs-table-header)
                             :width 13 :align 'right
                             :formatter (lambda (octets)
                                          (if octets
                                              (file-size-human-readable octets 'iec " ")
                                            "")))
                            (make-pgmacstbl-column
                             :name (propertize "Owner"
                                               'face 'pgmacs-table-header
                                               'help-echo "The owner of the table")
                             :width 13 :align 'right)
                            (make-pgmacstbl-column
                             :name (propertize "Comment" 'face 'pgmacs-table-header)
                             :width pgmacs-row-list-comment-width :align 'left))
                  :row-colors pgmacs-row-colors
                  :face 'pgmacs-table-data
                  :objects (pgmacs--list-tables)
                  :keymap pgmacs-table-list-map/table
                  :getter (lambda (object column pgmacstbl)
                            (pcase (pgmacstbl-column pgmacstbl column)
                              ("Table" (pgmacs--display-table-name (cl-first object)))
                              ("Rows" (cl-second object))
                              ("Size on disk" (cl-third object))
                              ("Owner" (cl-fourth object))
                              ("Comment" (cl-fifth object)))))))
    (unless (member (pgcon-server-variant con) '(cratedb cockroachdb spanner ydb questdb materialize risingwave))
      (let* ((res (pg-exec con "SELECT current_user, pg_backend_pid(), pg_is_in_recovery()"))
             (row (pg-result res :tuple 0)))
        (insert (format "\nConnected to database %s%s as %s%s (pid %d %s)\n"
                        (pgmacs--maybe-svg-icon #'pgmacs--svg-icon-database)
                        dbname
                        (pgmacs--maybe-svg-icon #'pgmacs--svg-icon-user)
                        (cl-first row)
                        (cl-second row)
                        (if (cl-third row)
                            (propertize "RECOVERING" 'help-echo "Read-only replica server in hot_standby mode")
                          "PRIMARY"))))
      ;; PostgreSQL has a function pg_size_pretty() that we could also use, but it's not implemented
      ;; in all semi-compatible variants.
      ;;
      ;; TODO: we could implement pg_database_size ourselves for YugabyteDB as per https://yugabytedb.tips/display-ysql-database-size/
      (let* ((sql "SELECT pg_catalog.pg_database_size($1)")
             (res (pg-exec-prepared con sql `((,dbname . "text"))))
             (size (cl-first (pg-result res :tuple 0))))
        (when size
          ;; We later use this information to decide whether to prefer fast-but-unreliable or
          ;; slow-but-exact queries for table row count.
          (put 'pgmacs--con 'database-size size)
          (insert (format "Total database size: %s" (file-size-human-readable size 'iec " "))))))
    ;; Perhaps also display output from
    ;; select state, count(*) from pg_stat_activity where pid <> pg_backend_pid() group by 1 order by 1;'
    ;; see https://gitlab.com/posetgres-ai/postgresql-consulting/postgres-howtos/-/blob/main/0068_psql_shortcuts.md
    (insert "\n\n")
    (dolist (btn pgmacs-table-list-buttons)
      (when (pgmacs--insert btn)
        (insert "  ")
        (when (> (current-column) (min (window-width) 90))
          (insert "\n"))))
    (insert "\n\n")
    (pgmacstbl-insert pgmacstbl)
    ;; Now update the "estimated rows" column of the list of tables. We make these queries as a
    ;; second step because they might be slow on large tables.
    (dolist (row (pgmacstbl-objects pgmacstbl))
      (let* ((table (cl-first row))
             (estimated-count (pgmacs--estimate-row-count table))
             (updated-row (copy-sequence row)))
        (setf (nth 1 updated-row) estimated-count)
        (pgmacstbl-update-object pgmacstbl updated-row row)))
    (pgmacs--stop-progress-reporter)
    (run-hooks 'pgmacs-table-list-hook)))


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
                             (let ((con (pg-connect-plist (widget-value w-dbname)
                                                          (widget-value w-username)
                                                          :password (widget-value w-password)
                                                          :host (widget-value w-hostname)
                                                          :port (widget-value w-port)
                                                          :tls-options (widget-value w-tls))))
                               (pgmacs-open con)))
                   "Connect")
    (widget-insert "\n")
    (use-local-map widget-keymap)
    (widget-setup)
    (goto-char (point-min))
    (widget-forward 1)))


(pgmacstbl-register-mark-face :marked-for-deletion `(:background ,pgmacs-deleted-color))


(provide 'pgmacs)

;;; pgmacs.el ends here
