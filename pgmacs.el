;;; pgmacs.el --- Emacs is editing a PostgreSQL database  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024 Eric Marsden
;; Author: Eric Marsden <eric.marsden@risk-engineering.org>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (pg "0.29"))
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
(require 'pg)


(defgroup pgmacs nil
  "Edit a PostgreSQL database from Emacs."
  :prefix "pgmacs-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/emarsden/pgmacs/"))

(defface pgmacs-table-data
  '((t (:inherit fixed-pitch-serif)))
  "Face used to display data in a PGMacs database table."
  :group 'pgmacs)

(defface pgmacs-table-header
  '((t (:inherit bold :weight bold)))
  "Face used to display a PGMacs database table header."
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

;; TODO: perhaps distinguish here between the insertion of a row when the cursor is in the table
;; (keybinding set by make-vtable) and insertion of a row otherwise.
(keymap-set pgmacs-mode-map (kbd "q") 'bury-buffer)
(keymap-set pgmacs-mode-map (kbd "e") (lambda (&rest _ignored) (pgmacs-run-sql)))


(defun pgmacs-mode ()
  "Major mode for editing PostgreSQL database."
  (setq major-mode 'pgmacs-mode
        mode-name "PGMacs")
  ;; Not appropriate for user to type stuff into our buffers.
  (put 'pgmacs-mode 'mode-class 'special)
  (use-local-map pgmacs-mode-map)
  (run-mode-hooks 'pgmacs-mode-hook))

(defvar pgmacs-transient-map (make-sparse-keymap))

(keymap-set pgmacs-transient-map (kbd "q") 'kill-buffer)

(define-minor-mode pgmacs-transient-mode
  "Minor mode for transient PGMacs buffers"
  :global nil
  :init-value nil
  :keymap pgmacs-transient-map)

;; Used for copying and pasting rows
(defvar pgmacs--kill-ring nil)


;; TODO fold some of these into common structs
(defvar-local pgmacs--con nil)
(defvar-local pgmacs--table nil)
(defvar-local pgmacs--column-type-names nil)
(defvar-local pgmacs--offset nil)

(defun pgmacs--value-formatter (type-name)
  (cond ((or (string= type-name "timestamp")
             (string= type-name "timestamptz")
	     (string= type-name "date"))
         ;; these are represented as a `decode-time' structure
         (lambda (val) (format-time-string "%Y-%m-%dT%T" val)))
        ((string= type-name "bpchar") #'byte-to-string)
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

(defun pgmacs--read-value (name type prompt)
  (let* ((prompt (format prompt name type))
         (stringval (read-string prompt))
         (parser (pg-lookup-parser type)))
    (if parser
        (funcall parser stringval (pgcon-client-encoding pgmacs--con))
      stringval)))

(defun pgmacs--edit-row (row primary-keys)
  (when (null primary-keys)
    (error "Can't edit content of a table that has no PRIMARY KEY"))
  (let* ((table (vtable-current-table))
         (current-row (vtable-current-object))
         (cols (vtable-columns table))
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
    (let* ((new-value (pgmacs--read-value col-name col-type "Change %s (%s) to: "))
           (sql (format "UPDATE %s SET %s = $1 WHERE %s = $2"
                        (pg-escape-identifier pgmacs--table)
                        (pg-escape-identifier col-name)
                        (pg-escape-identifier pk)))
           (res (pg-exec-prepared pgmacs--con sql
                                  `((,new-value . ,col-type)
                                    (,pk-value . ,pk-col-type)))))
      (message "PostgreSQL> %s" (pg-result res :status))
      (let ((new-row (copy-sequence current-row)))
        (setf (nth col-id new-row) new-value)
        ;; vtable-update-object doesn't work, so insert then delete old row
        (vtable-insert-object table new-row current-row)
        (vtable-remove-object table current-row)))))

(defun pgmacs--delete-row (row primary-keys)
  (when (null primary-keys)
    (error "Can't edit content of a table that has no PRIMARY KEY"))
  (when (y-or-n-p (format "Really delete PostgreSQL row %s?" row))
    (let* ((table (vtable-current-table))
           (cols (vtable-columns table))
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
        (message "PostgreSQL> %s" (pg-result res :status)))
      (vtable-remove-object table row))))

(defun pgmacs--insert-row (_current-row)
  ;; TODO we need to handle the case where there is no existing vtable because the underlying SQL table is empty.
  (let* ((table (vtable-current-table))
         (cols (vtable-columns table))
         (col-names (list))
         (values (list))
         (value-types (list)))
    (dolist (col cols)
      (let* ((col-name (vtable-column-name col))
             (col-id (cl-position col-name cols :key #'vtable-column-name :test #'string=))
             (col-type (aref pgmacs--column-type-names col-id))
             (col-has-default (not (null (pg-column-default pgmacs--con pgmacs--table col-name)))))
        (unless col-has-default
          (let* ((val (pgmacs--read-value col-name col-type "Value for column %s (%s): ")))
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
      (message "PostgreSQL> %s" (pg-result res :status))
      ;; It's tempting to use vtable-insert-object here to avoid a full refresh of the table.
      ;; However, we don't know what values were chosen for any columns that have a default.
      (pgmacs--display-table pgmacs--table))))

;; Copy current row to our "kill ring".
(defun pgmacs--copy-row (current-row)
  (setq pgmacs--kill-ring (cons pgmacs--table current-row))
  (message "Row copied to PGMacs kill ring"))

;; Insert new row at current position based on content of our "kill ring".
(defun pgmacs--yank-row (_current-row)
  (unless pgmacs--kill-ring
    (error "PGMacs kill ring is empty"))
  (unless (eq (car pgmacs--kill-ring) pgmacs--table)
    (error "Can't paste into a different PostgreSQL table"))
  (message "Pasting row from PGMacs kill ring")
  ;; Insert a new row based on the copied row, but without specifying values for the columns that
  ;; have a default value
  (let* ((yanked-row (cdr pgmacs--kill-ring))
         (table (vtable-current-table))
         (cols (vtable-columns table))
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
      (message "PostgreSQL> %s" (pg-result res :status))
      ;; It's tempting to use vtable-insert-object here to avoid a full refresh of the table.
      ;; However, we don't know what values were chosen for any columns that have a default.
      ;; This means that we can't insert at the current-row position.
      (pgmacs--display-table pgmacs--table))))


;; We can also SELECT c.column_name, c.data_type
(defun pgmacs--table-primary-keys (con table)
  (let* ((sql (format "SELECT c.column_name
      FROM information_schema.table_constraints tc
      JOIN information_schema.constraint_column_usage AS ccu USING (constraint_schema, constraint_name) 
      JOIN information_schema.columns AS c ON c.table_schema = tc.constraint_schema
      AND tc.table_name = c.table_name AND ccu.column_name = c.column_name
      WHERE constraint_type = 'PRIMARY KEY' and tc.table_name = %s"
                      (pg-escape-literal table)))
         (res (pg-exec con sql)))
    (mapcar #'cl-first (pg-result res :tuples))))

;; Return a string with information about this column, like the type name, PRIMARY KEY, UNIQUE, etc.
(defun pgmacs--column-info (con table column)
  (let* ((sql "SELECT tc.constraint_type FROM information_schema.table_constraints tc
               JOIN information_schema.constraint_column_usage AS ccu USING (constraint_schema, constraint_name) 
               JOIN information_schema.columns AS c ON c.table_schema = tc.constraint_schema
               AND tc.table_name = c.table_name
               AND ccu.column_name = c.column_name
               WHERE tc.table_name = $1 AND c.column_name = $2")
         (res (pg-exec-prepared con sql
                                `((,table . "text") (,column . "text"))))
         (constraints (pg-result res :tuples))
         (res (pg-exec-prepared
               con
               "SELECT character_maximum_length FROM information_schema.columns
                WHERE table_name=$1 AND column_name=$2"
               `((,table . "text") (,column . "text"))))
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
    (when (cl-first maxlen)
      (push (format "max-len %s" (cl-first maxlen)) column-info))
    (unless (null defaults)
      (push (format "DEFAULT %s" defaults) column-info))
    (string-join (reverse column-info) ", ")))

;; TODO also include VIEWs
;;   SELECT * FROM information_schema.views
(defun pgmacs--list-tables ()
  (let ((entries (list)))
    (dolist (table (pg-tables pgmacs--con))
      (let* ((sql (format "SELECT COUNT(*), pg_size_pretty(pg_total_relation_size(%s)) FROM %s"
                          (pg-escape-literal table)
                          (pg-escape-identifier table)))
             (res (pg-exec pgmacs--con sql))
             (rows (cl-first (pg-result res :tuple 0)))
             (size (cl-second (pg-result res :tuple 0)))
             (owner (pg-table-owner pgmacs--con table))
             (comment (pg-table-comment pgmacs--con table)))
        (push (list table rows size owner (or comment "")) entries)))
    entries))


(defun pgmacs--make-column-displayer (help-echo)
  (lambda (fvalue max-width _table)
    (let ((truncated (if (> (string-pixel-width fvalue) max-width)
                         ;; TODO could include the ellipsis here
                         (vtable--limit-string fvalue max-width)
                       fvalue)))
      (propertize truncated 'help-echo help-echo))))


(defun pgmacs--table-to-csv (&rest _ignore)
  "Dump the current PostgreSQL table in CSV format into an Emacs buffer."
  (let* ((con pgmacs--con)
         (table pgmacs--table)
         (buf (get-buffer-create (format "*PostgreSQL CSV for %s*" table)))
         (sql (format "COPY %s TO STDOUT WITH (FORMAT CSV)" (pg-escape-identifier table))))
    (pop-to-buffer buf)
    (pgmacs-transient-mode)
    (pg-copy-to-buffer con sql buf)))

(defun pgmacs--add-primary-key (&rest _ignore)
  "Add a PRIMARY KEY to the current PostgreSQL table."
  (let ((pk (pgmacs--table-primary-keys pgmacs--con pgmacs--table)))
    (when pk
      (error "Table %s already has a primary key %s" pgmacs--table pk)))
  (cl-flet ((exists (name) (cl-find name (pg-columns pgmacs--con pgmacs--table))))
    (let* ((colname (or (cl-find-if-not #'exists (list "id" "idpk" "idcol" "pk" "_id"))
                        (error "Can't autogenerate a name for primary key")))
           (sql (format "ALTER TABLE %s ADD COLUMN %s BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY"
                        (pg-escape-identifier pgmacs--table)
                        (pg-escape-identifier colname))))
      (when (y-or-n-p (format "Really run this SQL? %s" sql))
        (let ((res (pg-exec pgmacs--con sql)))
          (message "PostgreSQL> %s" (pg-result res :status)))))))


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
  (let* ((con pgmacs--con))
    (pop-to-buffer-same-window (format "*PostgreSQL %s %s*" (pgcon-dbname con) table))
    (pgmacs-mode)
    (let* ((primary-keys (pgmacs--table-primary-keys con table))
           (owner (pg-table-owner con table))
           (comment (pg-table-comment con table))
           (offset (or pgmacs--offset 0))
           (portal (format "pgbp%s" (pg-escape-identifier table)))
           (sql (format "SELECT * FROM %s OFFSET %s"
                        (pg-escape-identifier table) offset))
           (res (pg-exec-prepared con sql (list) :max-rows pgmacs-row-limit :portal portal))
           (rows (pg-result res :tuples))
           (column-names (mapcar #'cl-first (pg-result res :attributes)))
           (column-type-oids (mapcar #'cl-second (pg-result res :attributes)))
           (column-type-names (mapcar #'pg--lookup-type-name column-type-oids))
           (column-meta (mapcar (lambda (col) (pgmacs--column-info con table col)) column-names))
           (column-formatters (mapcar #'pgmacs--value-formatter column-type-names))
           (value-widths (mapcar #'pgmacs--value-width column-type-names))
           (column-widths (cl-loop for w in value-widths
                                   for name in column-names
                                   collect (1+ (max w (length name)))))
           (columns (cl-loop for name in column-names
                             for meta in column-meta
                             for fmt in column-formatters
                             for w in column-widths
                             collect (make-vtable-column
                                      :name (propertize name 'face 'pgmacs-table-header 'help-echo meta)
                                      :min-width (1+ (max w (length name)))
                                      :formatter fmt
                                      :displayer (pgmacs--make-column-displayer meta))))
           (inhibit-read-only t)
           (vtable (make-vtable
                    :insert nil
                    :use-header-line nil
                    :face 'pgmacs-table-data
                    :columns columns
                    :row-colors pgmacs-row-colors
                    :separator-width 5
                    :divider-width "5px"
                    :objects rows
                    :actions `("RET" (lambda (row) (pgmacs--edit-row row ',primary-keys))
                               "d" (lambda (row) (pgmacs--delete-row row ',primary-keys))
                               "i" pgmacs--insert-row
                               "k" pgmacs--copy-row
                               "y" pgmacs--yank-row
                               "e" (lambda (&rest _ignored) (pgmacs-run-sql))
                               "r" pgmacs--revert-vtable
                               "q" (lambda (&rest ignore) (kill-buffer))))))
      (erase-buffer)
      ;; (setq-local revert-buffer-function #'pgmacs-regenerate-display-table)
      (setq-local pgmacs--con con
                  pgmacs--table table
                  pgmacs--offset offset
                  pgmacs--column-type-names (apply #'vector column-type-names)
                  buffer-read-only t
                  truncate-lines t)
      (insert (propertize (format "PostgreSQL table %s, owned by %s\n" table owner) 'face 'bold))
      (when comment
        (insert (propertize "Comment" 'face 'bold))
        (insert (format ": %s" comment)))
      (let* ((sql (format "SELECT pg_size_pretty(pg_total_relation_size(%s)),
                                  pg_size_pretty(pg_indexes_size(%s))"
                          (pg-escape-literal table)
                          (pg-escape-literal table)))
             (row (pg-result (pg-exec con sql) :tuple 0)))
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
      (insert "\n\n")
      (when (pg-result res :incomplete)
        (when (> pgmacs--offset pgmacs-row-limit)
          (insert-text-button
           (format "Prev. %s rows" pgmacs-row-limit)
           'action (lambda (&rest _ignore)
                     (cl-decf pgmacs--offset pgmacs-row-limit)
                     (pgmacs--display-table table)))
          (insert "   "))
        (insert-text-button
         (format "Next %s rows" pgmacs-row-limit)
         'action (lambda (&rest _ignore)
                   (cl-incf pgmacs--offset pgmacs-row-limit)
                   (pgmacs--display-table table)))
        (insert "\n\n"))
      (if (null rows)
          (insert "(no rows in table)")
        (vtable-insert vtable)))))

(defun pgmacs--revert-vtable (&rest _ignore)
  "Redraw the table in the current buffer."
  ;; We are assuming there that there is a single vtable in the buffer.
  (goto-char (point-max))
  (vtable-beginning-of-table)
  (vtable-revert))

(defun pgmacs--display-backend-information (&rest _ignore)
  (let ((con pgmacs--con))
    (pop-to-buffer (get-buffer-create "*PostgreSQL backend information*"))
    (pgmacs-transient-mode)
    (let* ((res (pg-exec con "SELECT inet_server_addr(), inet_server_port(), pg_backend_pid()"))
           (row (pg-result res :tuple 0)))
      (insert (apply #'format "Database running on %s:%s with pid %s\n" row)))
    (let* ((res (pg-exec con "SELECT current_user"))
           (row (pg-result res :tuple 0)))
      (insert (apply #'format "Connected as user %s\n" row)))
    (let* ((res (pg-exec con "SELECT pg_postmaster_start_time()"))
           (dtime (car (pg-result res :tuple 0)))
           (fmt (funcall (pgmacs--value-formatter "timestamp") dtime)))
      (insert (format "Backend started at %s\n" fmt)))))

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
  (pop-to-buffer (get-buffer-create "*PostgreSQL TMP*"))
  (pgmacs-mode)
  (setq-local pgmacs--con con
              truncate-lines t)
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
                                    :name name
                                    :min-width (1+ (max w (length name)))
                                    :formatter fmt)))
         (inhibit-read-only t)
         (vtable (make-vtable
                  :insert nil
                  :use-header-line nil
                  :face 'pgmacs-table-data
                  :columns columns
                  :row-colors pgmacs-row-colors
                  :separator-width 5
                  :divider-width "5px"
                  :objects rows
                  :actions `("e" (lambda (&rest _ignored) (pgmacs-run-sql))
                             "q" (lambda (&rest _ignore) (kill-buffer))))))
    (erase-buffer)
    (remove-overlays)
    (insert (propertize "PostgreSQL query output" 'face 'bold))
    (insert "\n")
    (insert (propertize "SQL" 'face 'bold))
    (insert (format ": %s\n\n" sql))
    (if (null rows)
        (insert "(no rows)")
      (vtable-insert vtable))))

;;;###autoload
(defun pgmacs-open (con)
  "Browse the contents of PostgreSQL database to which we are connected over CON."
  (pop-to-buffer-same-window (format "*PostgreSQL %s*" (pgcon-dbname con)))
  (pgmacs-mode)
  (setq-local pgmacs--con con
              buffer-read-only t
              truncate-lines t)
  (set-process-query-on-exit-flag (pgcon-process con) nil)
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
                             :align 'left)
                            (make-vtable-column
                             :name (propertize "Rows" 'face 'pgmacs-table-header)
                             :width 7 :align 'right)
                            (make-vtable-column
                             :name (propertize "Size on disk" 'face 'pgmacs-table-header)
                             :width 13 :align 'right)
                            (make-vtable-column
                             :name (propertize "Owner" 'face 'pgmacs-table-header)
                             :width 13 :align 'left)
                            (make-vtable-column
                             :name (propertize "Comment" 'face 'pgmacs-table-header)
                             :width 30 :align 'left))
                  :row-colors pgmacs-row-colors
                  :face 'pgmacs-table-data
                  ;; :column-colors '("#202020" "#404040")
                  ;; :separator-width 5
                  :divider-width "2px"
                  :objects (pgmacs--list-tables)
                  :actions '("RET" (lambda (table-rows) (pgmacs--display-table (car table-rows)))
                             "e" (lambda (&rest _ignored) (pgmacs-run-sql))
                             "q"  (lambda (&rest _ignored) (kill-buffer)))
                  :getter (lambda (object column vtable)
                            (pcase (vtable-column vtable column)
                              ("Table" (cl-first object))
                              ("Rows" (cl-second object))
                              ("Size on disk" (cl-third object))
                              ("Owner" (cl-fourth object))
                              ("Comment" (cl-fifth object)))))))
    (erase-buffer)
    (insert (pg-backend-version con))
    (let* ((res (pg-exec con "SELECT current_user, pg_backend_pid(), pg_is_in_recovery()"))
           (row (pg-result res :tuple 0)))
      (insert (format "\nConnected to database %s as user %s (pid %d %s)\n"
                      dbname
                      (cl-first row)
                      (cl-second row)
                      (if (cl-third row) "RECOVERING" "PRIMARY"))))
    (let* ((sql (format "SELECT pg_size_pretty(pg_database_size(%s))"
                        (pg-escape-literal dbname)))
           (res (pg-exec con sql))
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
    (vtable-insert vtable)))


;;;###autoload
(defun pgmacs-open/string (connection-string)
  (interactive "sPostgreSQL connection string: ")
  (pgmacs-open (pg-connect/string connection-string)))

;;;###autoload
(defun pgmacs-open/uri (connection-uri)
  (interactive "sPostgreSQL connection URI: ")
  (pgmacs-open (pg-connect/uri connection-uri)))


;;;###autoload
(defun pgmacs ()
  (interactive)
  (require 'widget)
  (switch-to-buffer "*PGMacs connection widget*")
  (kill-all-local-variables)
  (remove-overlays)
  (widget-insert (propertize "Connect to PostgreSQL database" 'face 'bold))
  (widget-insert "\n\n")
  (let* ((w-dbname
         (progn
           (insert (format "%18s: " "Database name"))
           (widget-create 'editable-field
                          :size 20)))
        (w-hostname
         (progn
           (insert (format "\n%18s: " "Hostname"))
           (widget-create 'editable-field
                          :help-echo "The host where PostgreSQL is running"
                          :default ""
                          :size 20)))
        (w-port
         (progn
           (insert (format "\n%18s: " "Port"))
           (widget-create 'natnum
                          :format "%v"
                          :size 20
                          "5432")))
        (w-username
         (progn
           (insert (format "\n%18s: " "Username"))
           (widget-create 'editable-field
                          :help-echo "Authenticate as this user"
                          :size 20)))
        (w-password
         (progn
           (insert (format "\n%18s: " "Password"))
           (widget-create 'editable-field
                          :size 20)))
        (w-tls
         (progn
           (insert (format "\n%18s: " "TLS encryption"))
           (widget-create 'checkbox
                          :help-echo "Whether to use an encrypted connection"))))
    (widget-insert "\n\n")
    (widget-create 'push-button
                   :notify (lambda (&rest _ignore)
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
