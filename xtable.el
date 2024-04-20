;;; xtable.el --- Displaying data in tables  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.
;; Copyright (C) 2024 Eric Marsden
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This library is distributed under the terms of the GNU General Public License
;; as published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;;; Commentary:

;; This code is a fork of the vtable library included with Emacs 29, written by
;; Lars Ingebrigtsen. It includes modifications/fixes for a few issues that
;; arise when including a vtable in a buffer which also includes other content.
;; Some of these fixes will hopefully be fed back upstream once this library has
;; been stabilized.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'text-property-search)
(require 'mule-util)

(defface xtable
  '((t :inherit variable-pitch))
  "Face used (by default) for xtables."
  :version "29.1"
  :group 'faces)

(cl-defstruct xtable-column
  "A xtable column."
  name
  width
  min-width
  max-width
  primary
  align
  getter
  formatter
  displayer
  -numerical)

(defclass xtable ()
  ((columns :initarg :columns :accessor xtable-columns)
   (objects :initarg :objects :accessor xtable-objects)
   (objects-function :initarg :objects-function
                     :accessor xtable-objects-function)
   (getter :initarg :getter :accessor xtable-getter)
   (formatter :initarg :formatter :accessor xtable-formatter)
   (displayer :initarg :displayer :accessor xtable-displayer)
   (use-header-line :initarg :use-header-line
                    :accessor xtable-use-header-line)
   (face :initarg :face :accessor xtable-face)
   (actions :initarg :actions :accessor xtable-actions)
   (keymap :initarg :keymap :accessor xtable-keymap)
   (separator-width :initarg :separator-width :accessor xtable-separator-width)
   (divider :initarg :divider :accessor xtable-divider :initform nil)
   (sort-by :initarg :sort-by :accessor xtable-sort-by)
   (ellipsis :initarg :ellipsis :accessor xtable-ellipsis)
   (column-colors :initarg :column-colors :accessor xtable-column-colors)
   (row-colors :initarg :row-colors :accessor xtable-row-colors)
   (-cached-colors :initform nil)
   (-cache :initform (make-hash-table :test #'equal))
   (-cached-keymap :initform nil)
   (-has-column-spec :initform nil))
  "An object to hold the data for a table.")

(defvar-keymap xtable-map
  "S" #'xtable-sort-by-current-column
  "{" #'xtable-narrow-current-column
  "}" #'xtable-widen-current-column
  "g" #'xtable-revert-command
  "M-<left>" #'xtable-previous-column
  "M-<right>" #'xtable-next-column)

(defvar-keymap xtable-header-line-map
  :parent xtable-map
  "<follow-link>" 'mouse-face
  "<mouse-2>" #'xtable-header-line-sort)

(cl-defun make-xtable (&key columns objects objects-function
                            getter
                            formatter
                            displayer
                            (use-header-line t)
                            (face 'xtable)
                            actions keymap
                            (separator-width 1)
                            divider
                            divider-width
                            sort-by
                            (ellipsis t)
                            (insert t)
                            row-colors
                            column-colors)
  "Create and insert a xtable at point.
The xtable object is returned.  If INSERT is nil, the table won't
be inserted.

See info node `(xtable)Top' for xtable documentation."
  (when objects-function
    (setq objects (funcall objects-function)))
  ;; We'll be altering the list, so create a copy.
  (setq objects (copy-sequence objects))
  (let ((table
         (make-instance
          'xtable
          :objects objects
          :objects-function objects-function
          :getter getter
          :formatter formatter
          :displayer displayer
          :use-header-line use-header-line
          :face face
          :actions actions
          :keymap keymap
          :separator-width separator-width
          :sort-by sort-by
          :row-colors row-colors
          :column-colors column-colors
          :ellipsis ellipsis)))
    ;; Store whether the user has specified columns or not.
    (setf (slot-value table '-has-column-spec) (not (not columns)))
    ;; Auto-generate the columns.
    (unless columns
      (unless objects
        (error "Can't auto-generate columns; no objects"))
      (setq columns (make-list (length (car objects)) "")))
    (setf (xtable-columns table)
          (mapcar (lambda (column)
                    (cond
                     ;; We just have the name (as a string).
                     ((stringp column)
                      (make-xtable-column :name column))
                     ;; A plist of keywords/values.
                     ((listp column)
                      (apply #'make-xtable-column column))
                     ;; A full `xtable-column' object.
                     (t
                      column)))
                  columns))
    ;; Compute missing column data.
    (setf (xtable-columns table) (xtable--compute-columns table))
    ;; Compute the colors.
    (when (or row-colors column-colors)
      (setf (slot-value table '-cached-colors)
            (xtable--compute-colors row-colors column-colors)))
    ;; Compute the divider.
    (when (or divider divider-width)
      (setf (xtable-divider table)
            (propertize
             (or (copy-sequence divider)
                 (propertize
                  " " 'display
                  (list 'space :width
                        (list (xtable--compute-width table divider-width)))))
             'mouse-face 'highlight
             'keymap
             (define-keymap
               "<drag-mouse-1>" #'xtable--drag-resize-column
               "<down-mouse-1>" #'ignore))))
    ;; Compute the keymap.
    (setf (slot-value table '-cached-keymap) (xtable--make-keymap table))
    (unless sort-by
      (seq-do-indexed (lambda (column index)
                        (when (xtable-column-primary column)
                          (push (cons index (xtable-column-primary column))
                                (xtable-sort-by table))))
                      (xtable-columns table)))
    (when insert
      (xtable-insert table))
    table))

(defun xtable--compute-colors (row-colors column-colors)
  (cond
   ((null column-colors)
    (mapcar #'xtable--make-color-face row-colors))
   ((null row-colors)
    (mapcar #'xtable--make-color-face column-colors))
   (t
    (cl-loop for row in row-colors
             collect (cl-loop for column in column-colors
                              collect (xtable--face-blend
                                       (xtable--make-color-face row)
                                       (xtable--make-color-face column)))))))

(defun xtable--make-color-face (object)
  (if (stringp object)
      (list :background object)
    object))

(defun xtable--face-blend (face1 face2)
  (let ((foreground (xtable--face-color face1 face2 #'face-foreground
                                        :foreground))
        (background (xtable--face-color face1 face2 #'face-background
                                        :background)))
    `(,@(and foreground (list :foreground foreground))
      ,@(and background (list :background background)))))

(defun xtable--face-color (face1 face2 accessor slot)
  (let ((col1 (if (facep face1)
                  (funcall accessor face1)
                (plist-get face1 slot)))
        (col2 (if (facep face2)
                  (funcall accessor face2)
                (plist-get face2 slot))))
    (if (and col1 col2)
        (xtable--color-blend col1 col2)
      (or col1 col2))))

;;; FIXME: This is probably not the right way to blend two colors, is
;;; it?
(defun xtable--color-blend (color1 color2)
  (cl-destructuring-bind (r g b)
      (mapcar (lambda (n) (* (/ n 2) 255.0))
              (cl-mapcar #'+ (color-name-to-rgb color1)
                         (color-name-to-rgb color2)))
    (format "#%02X%02X%02X" r g b)))

;;; Interface utility functions.

(defun xtable-current-table ()
  "Return the table under point."
  (get-text-property (point) 'xtable))

(defun xtable-current-object ()
  "Return the object under point."
  (get-text-property (point) 'xtable-object))

(defun xtable-current-column ()
  "Return the index of the column under point."
  (get-text-property (point) 'xtable-column))

(defun xtable-beginning-of-table ()
  "Go to the start of the current table."
  (goto-char (point-max))
  (if (text-property-search-backward 'xtable)
      (point)
    (goto-char (point-min))))

(defun xtable-end-of-table ()
  "Go to the end of the current table."
  (goto-char (point-min))
  (if (text-property-search-forward 'xtable)
      (point)
    (goto-char (point-max))))

(defun xtable-goto-object (object)
  "Go to OBJECT in the current table.
Return the position of the object if found, and nil if not."
  (let ((start (point)))
    (xtable-beginning-of-table)
    (save-restriction
      (narrow-to-region (point) (save-excursion (xtable-end-of-table)))
      (if (text-property-search-forward 'xtable-object object #'eq)
          (progn
            (forward-line -1)
            (point))
        (goto-char start)
        nil))))

(defun xtable-goto-table (table)
  "Go to TABLE in the current buffer.
If TABLE is found, return the position of the start of the table.
If it can't be found, return nil and don't move point."
  (let ((start (point)))
    (goto-char (point-min))
    (if-let ((match (text-property-search-forward 'xtable table t)))
        (goto-char (prop-match-beginning match))
      (goto-char start)
      nil)))

(defun xtable-goto-column (column)
  "Go to COLUMN on the current line."
  (beginning-of-line)
  (if-let ((match (text-property-search-forward 'xtable-column column t)))
      (goto-char (prop-match-beginning match))
    (end-of-line)))

(defun xtable-update-object (table object old-object)
  "Replace OLD-OBJECT in TABLE with OBJECT."
  (let* ((objects (xtable-objects table))
         (inhibit-read-only t))
    ;; First replace the object in the object storage.
    (if (eq old-object (car objects))
        ;; It's at the head, so replace it there.
        (setf (xtable-objects table)
              (cons object (cdr objects)))
      ;; Otherwise splice into the list.
      (while (and (cdr objects)
                  (not (eq (cadr objects) old-object)))
        (setq objects (cdr objects)))
      (unless objects
        (error "Can't find the old object"))
      (setcar (cdr objects) object))
    ;; Then update the cache...
    (let* ((line-number (seq-position old-object (car (xtable--cache table))))
           (line (elt (car (xtable--cache table)) line-number)))
      (unless line
        (error "Can't find cached object"))
      (setcar line object)
      (setcdr line (xtable--compute-cached-line table object))
      ;; ... and redisplay the line in question.
      (save-excursion
        (xtable-goto-object old-object)
        (let ((keymap (get-text-property (point) 'keymap))
              (start (point)))
          (delete-line)
          (xtable--insert-line table line line-number
                               (nth 1 (xtable--cache table))
                               (xtable--spacer table))
          (add-text-properties start (point) (list 'keymap keymap
                                                   'xtable table))))
      ;; We may have inserted a non-numerical value into a previously
      ;; all-numerical table, so recompute.
      (xtable--recompute-numerical table (cdr line)))))

(defun xtable-remove-object (table object)
  "Remove OBJECT from TABLE.
This will also remove the displayed line."
  ;; First remove from the objects.
  (setf (xtable-objects table) (delq object (xtable-objects table)))
  ;; Then adjust the cache and display.
  (let ((cache (xtable--cache table))
        (inhibit-read-only t))
    (setcar cache (delq (assq object (car cache)) (car cache)))
    (save-excursion
      (xtable-goto-table table)
      (when (xtable-goto-object object)
        (delete-line)))))

(defun xtable-insert-object (table object &optional after-object)
  "Insert OBJECT into TABLE after AFTER-OBJECT.
If AFTER-OBJECT is nil (or doesn't exist in the table), insert
OBJECT at the end.
This also updates the displayed table."
  ;; First insert into the objects.
  (let (pos)
    (if (and after-object
             (setq pos (memq after-object (xtable-objects table))))
        ;; Splice into list.
        (setcdr pos (cons object (cdr pos)))
      ;; Append.
      (nconc (xtable-objects table) (list object))))
  ;; Then adjust the cache and display.
  (save-excursion
    (xtable-goto-table table)
    (let* ((cache (xtable--cache table))
           (inhibit-read-only t)
           (keymap (get-text-property (point) 'keymap))
           (ellipsis (if (xtable-ellipsis table)
                         (propertize (truncate-string-ellipsis)
                                     'face (xtable-face table))
                       ""))
           (ellipsis-width (string-pixel-width ellipsis))
           (elem (and after-object
                      (assq after-object (car cache))))
           (line (cons object (xtable--compute-cached-line table object))))
      (if (not elem)
          ;; Append.
          (progn
            (setcar cache (nconc (car cache) (list line)))
            (xtable-end-of-table))
        ;; Splice into list.
        (let ((pos (memq elem (car cache))))
          (setcdr pos (cons line (cdr pos)))
          (unless (xtable-goto-object after-object)
            (xtable-end-of-table))))
      (let ((start (point)))
        ;; FIXME: We have to adjust colors in lines below this if we
        ;; have :row-colors.
        (xtable--insert-line table line 0
                             (nth 1 cache) (xtable--spacer table)
                             ellipsis ellipsis-width)
        (add-text-properties start (point) (list 'keymap keymap
                                                 'xtable table)))
      ;; We may have inserted a non-numerical value into a previously
      ;; all-numerical table, so recompute.
      (xtable--recompute-numerical table (cdr line)))))

(defun xtable-column (table index)
  "Return the name of the INDEXth column in TABLE."
  (xtable-column-name (elt (xtable-columns table) index)))

;;; Generating the table.

(defun xtable--get-value (object index column table)
  "Compute a cell value."
  (cond
   ((xtable-column-getter column)
    (funcall (xtable-column-getter column)
             object table))
   ((xtable-getter table)
    (funcall (xtable-getter table)
             object index table))
   ;; No getter functions; standard getters.
   ((stringp object)
    object)
   (t
    (elt object index))))

(defun xtable--compute-columns (table)
  (let ((numerical (make-vector (length (xtable-columns table)) t))
        (columns (xtable-columns table)))
    ;; First determine whether there are any all-numerical columns.
    (dolist (object (xtable-objects table))
      (seq-do-indexed
       (lambda (_elem index)
         (unless (numberp (xtable--get-value object index (elt columns index)
                                             table))
           (setf (elt numerical index) nil)))
       (xtable-columns table)))
    ;; Then fill in defaults.
    (seq-map-indexed
     (lambda (column index)
       ;; This is used when displaying.
       (unless (xtable-column-align column)
         (setf (xtable-column-align column)
               (if (elt numerical index)
                   'right
                 'left)))
       ;; This is used for sorting.
       (setf (xtable-column--numerical column)
             (elt numerical index))
       column)
     (xtable-columns table))))

(defun xtable--spacer (table)
  (xtable--compute-width table (xtable-separator-width table)))

(defun xtable--recompute-cache (table)
  (let* ((data (xtable--compute-cache table))
         (widths (xtable--compute-widths table data)))
    (setf (gethash (xtable--cache-key) (slot-value table '-cache))
          (list data widths))))

(defun xtable--ensure-cache (table)
  (or (xtable--cache table)
      (xtable--recompute-cache table)))

(defun xtable-insert (table)
  (let* ((spacer (xtable--spacer table))
         (start (point))
         (ellipsis (if (xtable-ellipsis table)
                       (propertize (truncate-string-ellipsis)
                                   'face (xtable-face table))
                     ""))
         (ellipsis-width (string-pixel-width ellipsis))
         ;; We maintain a cache per screen/window width, so that we render
         ;; correctly if Emacs is open on two different screens (or the
         ;; user resizes the frame).
         (widths (nth 1 (xtable--ensure-cache table))))
    ;; Don't insert any header or header line if the user hasn't
    ;; specified the columns.
    (when (slot-value table '-has-column-spec)
      (if (xtable-use-header-line table)
          (xtable--set-header-line table widths spacer)
        ;; Insert the header line directly into the buffer, and put a
        ;; keymap to be able to sort the columns there (by clicking on
        ;; them).
        (xtable--insert-header-line table widths spacer)
        (add-text-properties start (point)
                             (list 'keymap xtable-header-line-map
                                   'rear-nonsticky t
                                   'xtable table))
        (setq start (point))))
    (xtable--sort table)
    ;; Insert the data.
    (let ((line-number 0))
      (dolist (line (car (xtable--cache table)))
        (xtable--insert-line table line line-number widths spacer
                             ellipsis ellipsis-width)
        (setq line-number (1+ line-number))))
    (add-text-properties start (point)
                         (list 'rear-nonsticky t
                               'xtable table))
    (goto-char start)))

(defun xtable--insert-line (table line line-number widths spacer
                                  &optional ellipsis ellipsis-width)
  (let ((start (point))
        (columns (xtable-columns table))
        (column-colors
         (and (xtable-column-colors table)
              (if (xtable-row-colors table)
                  (elt (slot-value table '-cached-colors)
                       (mod line-number (length (xtable-row-colors table))))
                (slot-value table '-cached-colors))))
        (divider (xtable-divider table))
        (keymap (slot-value table '-cached-keymap)))
    (seq-do-indexed
     (lambda (elem index)
       (let ((value (nth 0 elem))
             (column (elt columns index))
             (pre-computed (nth 2 elem)))
         ;; See if we have any formatters here.
         (cond
          ((xtable-column-formatter column)
           (setq value (funcall (xtable-column-formatter column) value)
                 pre-computed nil))
          ((xtable-formatter table)
           (setq value (funcall (xtable-formatter table)
                                value index table)
                 pre-computed nil)))
         (let ((displayed
                ;; Allow any displayers to have their say.
                (cond
                 ((xtable-column-displayer column)
                  (funcall (xtable-column-displayer column)
                           value (elt widths index) table))
                 ((xtable-displayer table)
                  (funcall (xtable-displayer table)
                           value index (elt widths index) table))
                 (pre-computed
                  ;; If we don't have a displayer, use the pre-made
                  ;; (cached) string value.
                  (if (> (nth 1 elem) (elt widths index))
                      (concat
                       (xtable--limit-string
                        pre-computed (- (elt widths index)
                                        (or ellipsis-width 0)))
                       ellipsis)
                    pre-computed))
                 ;; Recompute widths.
                 (t
                  (if (> (string-pixel-width value) (elt widths index))
                      (concat
                       (xtable--limit-string
                        value (- (elt widths index)
                                 (or ellipsis-width 0)))
                       ellipsis)
                    value))))
               (start (point))
               ;; Don't insert the separator after the final column.
               (last (= index (- (length line) 2))))
           (if (eq (xtable-column-align column) 'left)
               (progn
                 (insert displayed)
                 (insert (propertize
                          " " 'display
                          (list 'space
                                :width (list
                                        (+ (- (elt widths index)
                                              (string-pixel-width displayed))
                                           (if last 0 spacer)))))))
             ;; Align to the right.
             (insert (propertize " " 'display
                                 (list 'space
                                       :width (list (- (elt widths index)
                                                       (string-pixel-width
                                                        displayed)))))
                     displayed)
             (unless last
               (insert (propertize " " 'display
                                   (list 'space
                                         :width (list spacer))))))
           (put-text-property start (point) 'xtable-column index)
           (put-text-property start (point) 'keymap keymap)
           (when column-colors
             (add-face-text-property
              start (point)
              (elt column-colors (mod index (length column-colors)))))
           (when divider
             (insert divider)
             (setq start (point))))))
     (cdr line))
    (insert "\n")
    (put-text-property start (point) 'xtable-object (car line))
    (unless column-colors
      (when-let ((row-colors (slot-value table '-cached-colors)))
        (add-face-text-property
         start (point)
         (elt row-colors (mod line-number (length row-colors))))))))

(defun xtable--cache-key ()
  (cons (frame-terminal) (window-width)))

(defun xtable--cache (table)
  (gethash (xtable--cache-key) (slot-value table '-cache)))

(defun xtable--clear-cache (table)
  (setf (gethash (xtable--cache-key) (slot-value table '-cache)) nil))

(defun xtable--sort (table)
  (pcase-dolist (`(,index . ,direction) (xtable-sort-by table))
    (let ((cache (xtable--cache table))
          (numerical (xtable-column--numerical
                      (elt (xtable-columns table) index)))
          (numcomp (if (eq direction 'descend)
                       #'> #'<))
          (stringcomp (if (eq direction 'descend)
                          #'string> #'string<)))
      (setcar cache
              (sort (car cache)
                    (lambda (e1 e2)
                      (let ((c1 (elt e1 (1+ index)))
                            (c2 (elt e2 (1+ index))))
                        (if numerical
                            (funcall numcomp (car c1) (car c2))
                          (funcall
                           stringcomp
                           (if (stringp (car c1))
                               (car c1)
                             (format "%s" (car c1)))
                           (if (stringp (car c2))
                               (car c2)
                             (format "%s" (car c2))))))))))))

(defun xtable--indicator (table index)
  (let ((order (car (last (xtable-sort-by table)))))
    (if (eq index (car order))
        ;; We're sorting by this column last, so return an indicator.
        (catch 'found
          (dolist (candidate (nth (if (eq (cdr order) 'ascend)
                                      1
                                    0)
                                  '((?▼ ?v)
                                    (?▲ ?^))))
            (when (char-displayable-p candidate)
              (throw 'found (string candidate)))))
      "")))

(defun xtable--insert-header-line (table widths spacer)
  (cl-flet ((space-for (width)
              (propertize " " 'display (list 'space :width (list width)))))
    (let ((start (point))
          (divider (xtable-divider table))
          (cmap (define-keymap
                  "<header-line> <drag-mouse-1>" #'xtable--drag-resize-column
                  "<header-line> <down-mouse-1>" #'ignore))
          (dmap (define-keymap
                  "<header-line> <drag-mouse-1>"
                  (lambda (e)
                    (interactive "e")
                    (xtable--drag-resize-column e t))
                  "<header-line> <down-mouse-1>" #'ignore)))
      (seq-do-indexed
       (lambda (column index)
         (let* ((name (propertize
                       (xtable-column-name column)
                       'face (list 'pgmacs-table-header)
                       'mouse-face 'header-line-highlight
                       'keymap cmap))
                (start (point))
                (indicator (xtable--indicator table index))
                (indicator-width (string-pixel-width indicator))
                (last (= index (1- (length (xtable-columns table)))))
                (displayed (if (> (string-pixel-width name)
                                  (- (elt widths index) indicator-width))
                               (xtable--limit-string
                                name (- (elt widths index) indicator-width))
                             name)))
           (let ((fill-width (- (elt widths index)
                                (string-pixel-width displayed)
                                indicator-width)))
             (if (eq (xtable-column-align column) 'left)
                 (insert displayed (space-for fill-width) indicator)
               (insert (space-for fill-width) displayed indicator)))
           (unless last
             (insert (space-for spacer)))
           (when (and divider (not last))
             (insert (propertize divider 'keymap dmap)))
           (put-text-property start (point) 'xtable-column index)))
       (xtable-columns table))
      (insert "\n")
      (add-face-text-property start (point) 'header-line))))

(defun xtable--drag-resize-column (e &optional next)
  "Resize the column by dragging.
If NEXT, do the next column."
  (interactive "e")
  (let* ((pos-start (event-start e))
	 (obj (posn-object pos-start)))
    (with-current-buffer (window-buffer (posn-window pos-start))
      (let ((column
             ;; In the header line we have a text property on the
             ;; divider.
             (or (get-text-property (if obj (cdr obj)
                                      (posn-point pos-start))
			            'xtable-column
			            (car obj))
                 ;; For reasons of efficiency, we don't have that in
                 ;; the buffer itself, so find the column.
                 (save-excursion
                   (goto-char (posn-point pos-start))
                   (1+
                    (get-text-property
                     (prop-match-beginning
                      (text-property-search-backward 'xtable-column))
                     'xtable-column)))))
            (start-x (car (posn-x-y pos-start)))
            (end-x (car (posn-x-y (event-end e)))))
        (when (or (> column 0) next)
          (xtable--alter-column-width (xtable-current-table)
                                      (if next
                                          column
                                        (1- column))
                                      (- end-x start-x)))))))

(defun xtable--recompute-numerical (table line)
  "Recompute numericalness of columns if necessary."
  (let ((columns (xtable-columns table))
        (recompute nil))
    (seq-do-indexed
     (lambda (elem index)
       (when (and (xtable-column--numerical (elt columns index))
                  (not (numberp elem)))
         (setq recompute t)))
     line)
    (when recompute
      (xtable--compute-columns table))))

(defun xtable--set-header-line (table widths spacer)
  (setq header-line-format
        (string-replace
         "%" "%%"
         (with-temp-buffer
           (insert " ")
           (xtable--insert-header-line table widths spacer)
           ;; Align the header with the (possibly) fringed buffer text.
           (put-text-property
            (point-min) (1+ (point-min))
            'display '(space :align-to 0))
           (buffer-substring (point-min) (1- (point-max))))))
  (xtable-header-mode 1))

(defun xtable--limit-string (string pixels)
  (while (and (length> string 0)
              (> (string-pixel-width string) pixels))
    (setq string (substring string 0 (1- (length string)))))
  string)

(defun xtable--char-width (table)
  (string-pixel-width (propertize "x" 'face (xtable-face table))))

(defun xtable--compute-width (table spec)
  (cond
   ((numberp spec)
    (* spec (xtable--char-width table)))
   ((string-match "\\([0-9.]+\\)ex" spec)
    (* (string-to-number (match-string 1 spec)) (xtable--char-width table)))
   ((string-match "\\([0-9.]+\\)px" spec)
    (string-to-number (match-string 1 spec)))
   ((string-match "\\([0-9.]+\\)%" spec)
    (/ (* (string-to-number (match-string 1 spec)) (window-width nil t))
       100))
   (t
    (error "Invalid spec: %s" spec))))

(defun xtable--compute-widths (table cache)
  "Compute the display widths for TABLE."
  (seq-into
   (seq-map-indexed
    (lambda (column index)
      (let ((width
             (or
              ;; Explicit widths.
              (and (xtable-column-width column)
                   (xtable--compute-width table (xtable-column-width column)))
              ;; Compute based on the displayed widths of
              ;; the data.
              (seq-max (seq-map (lambda (elem)
                                  (nth 1 (elt (cdr elem) index)))
                                cache)))))
        ;; Let min-width/max-width specs have their say.
        (when-let ((min-width (and (xtable-column-min-width column)
                                   (xtable--compute-width
                                    table (xtable-column-min-width column)))))
          (setq width (max width min-width)))
        (when-let ((max-width (and (xtable-column-max-width column)
                                   (xtable--compute-width
                                    table (xtable-column-max-width column)))))
          (setq width (min width max-width)))
        width))
    (xtable-columns table))
   'vector))

(defun xtable--compute-cache (table)
  (seq-map
   (lambda (object)
     (cons object (xtable--compute-cached-line table object)))
   (xtable-objects table)))

(defun xtable--compute-cached-line (table object)
  (seq-map-indexed
   (lambda (column index)
     (let* ((value (xtable--get-value object index column table))
            (string (if (stringp value)
                        (copy-sequence value)
                      (format "%s" value))))
       (add-face-text-property 0 (length string)
                               (xtable-face table)
                               t string)
       ;; We stash the computed width and string here -- if there are
       ;; no formatters/displayers, we'll be using the string, and
       ;; then won't have to recreate it.
       (list value (string-pixel-width string) string)))
   (xtable-columns table)))

(defun xtable--make-keymap (table)
  (let ((map (if (or (xtable-actions table)
                     (xtable-keymap table))
                 (copy-keymap xtable-map)
               xtable-map)))
    (when-let ((actions (xtable-actions table)))
      (while actions
        (funcall (lambda (key binding)
                   (keymap-set map key
                               (lambda (object)
                                 (interactive (list (xtable-current-object)))
                                 (funcall binding object))))
                 (car actions) (cadr actions))
        (setq actions (cddr actions))))
    (if (xtable-keymap table)
        (progn
          (setf (xtable-keymap table)
                (copy-keymap (xtable-keymap table)))
          ;; Respect any previously set parent keymaps.
          (set-keymap-parent (xtable-keymap table)
                             (if (keymap-parent (xtable-keymap table))
                                 (append (ensure-list
                                          (xtable-keymap table))
                                         (list map))
                               map))
          (xtable-keymap table))
      map)))

(defun xtable-revert ()
  "Regenerate the table under point."
  (let ((table (xtable-current-table))
        (object (xtable-current-object))
        (column (xtable-current-column))
        (inhibit-read-only t))
    (unless table
      (user-error "No table under point"))
    (delete-region (xtable-beginning-of-table) (xtable-end-of-table))
    (xtable-insert table)
    (when object
      (xtable-goto-object object))
    (when column
      (xtable-goto-column column))))

(defun xtable--widths (table)
  (nth 1 (xtable--ensure-cache table)))

;;; Commands.

(defvar-keymap xtable-header-mode-map
  "<header-line> <mouse-1>" 'xtable-header-line-sort
  "<header-line> <mouse-2>" 'xtable-header-line-sort)

(define-minor-mode xtable-header-mode
  "Minor mode for buffers with xtables with headers."
  :keymap xtable-header-mode-map)

(defun xtable-narrow-current-column (&optional n)
  "Narrow the current column by N characters.
If N isn't given, N defaults to 1.

Interactively, N is the prefix argument."
  (interactive "p")
  (let* ((table (xtable-current-table))
         (column (xtable-current-column)))
    (unless column
      (user-error "No column under point"))
    (xtable--alter-column-width table column
                                (- (* (xtable--char-width table) (or n 1))))))

(defun xtable--alter-column-width (table column delta)
  (let ((widths (xtable--widths table)))
    (setf (aref widths column)
          (max (* (xtable--char-width table) 2)
               (+ (aref widths column) delta)))
    ;; Store the width so it'll be respected on a revert.
    (setf (xtable-column-width (elt (xtable-columns table) column))
          (format "%dpx" (aref widths column)))
    (xtable-revert)))

(defun xtable-widen-current-column (&optional n)
  "Widen the current column by N characters.
If N isn't given, N defaults to 1.

Interactively, N is the prefix argument."
  (interactive "p")
  (xtable-narrow-current-column (- n)))

(defun xtable-previous-column ()
  "Go to the previous column."
  (interactive)
  (xtable-goto-column
   (max 0 (1- (or (xtable-current-column)
                  (length (xtable--widths (xtable-current-table))))))))

(defun xtable-next-column ()
  "Go to the next column."
  (interactive)
  (when (xtable-current-column)
    (xtable-goto-column
     (min (1- (length (xtable--widths (xtable-current-table))))
          (1+ (xtable-current-column))))))

(defun xtable-revert-command ()
  "Re-query data and regenerate the table under point."
  (interactive)
  (let ((table (xtable-current-table)))
    (when (xtable-objects-function table)
      (setf (xtable-objects table) (funcall (xtable-objects-function table))))
    (xtable--clear-cache table))
  (xtable-revert))

(defun xtable-sort-by-current-column ()
  "Sort the table under point by the column under point."
  (interactive)
  (unless (xtable-current-column)
    (user-error "No current column"))
  (let* ((table (xtable-current-table))
         (last (car (last (xtable-sort-by table))))
         (index (xtable-current-column)))
    ;; First prune any previous appearance of this column.
    (setf (xtable-sort-by table)
          (delq (assq index (xtable-sort-by table))
                (xtable-sort-by table)))
    ;; Then insert this as the last sort key.
    (setf (xtable-sort-by table)
          (append (xtable-sort-by table)
                  (list (cons index
                              (if (eq (car last) index)
                                  (if (eq (cdr last) 'ascend)
                                      'descend
                                    'ascend)
                                'ascend))))))
  (xtable-revert))

(defun xtable-header-line-sort (e)
  "Sort a xtable from the header line."
  (interactive "e")
  (let* ((pos (event-start e))
	 (obj (posn-object pos)))
    (with-current-buffer (window-buffer (posn-window pos))
      (goto-char (point-min))
      (xtable-goto-column
       (get-text-property (if obj (cdr obj) (posn-point pos))
			  'xtable-column
			  (car obj)))
      (xtable-sort-by-current-column))))

(provide 'xtable)

;;; xtable.el ends here
