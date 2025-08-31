;;; pgmacstbl.el --- Displaying data in tables  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Free Software Foundation, Inc.
;; Copyright (C) 2024-2025 Eric Marsden
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
;;
;; For this reason, please don't use this code from outside of PGmacs.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'text-property-search)
(require 'mule-util)

(defface pgmacstbl
  '((t :inherit variable-pitch))
  "Face used (by default) for pgmacstbls."
  :version "29.1"
  :group 'faces)

(cl-defstruct pgmacstbl-column
  "A pgmacstbl column."
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


(defvar pgmacstbl--registered-mark-faces nil)

(defclass pgmacstbl ()
  ((columns :initarg :columns :accessor pgmacstbl-columns)
   (objects :initarg :objects :accessor pgmacstbl-objects)
   (objects-function :initarg :objects-function
                     :accessor pgmacstbl-objects-function)
   (getter :initarg :getter :accessor pgmacstbl-getter)
   (formatter :initarg :formatter :accessor pgmacstbl-formatter)
   (displayer :initarg :displayer :accessor pgmacstbl-displayer)
   (use-header-line :initarg :use-header-line
                    :accessor pgmacstbl-use-header-line)
   (face :initarg :face :accessor pgmacstbl-face)
   (actions :initarg :actions :accessor pgmacstbl-actions)
   (keymap :initarg :keymap :accessor pgmacstbl-keymap)
   (separator-width :initarg :separator-width :accessor pgmacstbl-separator-width)
   (divider :initarg :divider :accessor pgmacstbl-divider :initform nil)
   (sort-by :initarg :sort-by :accessor pgmacstbl-sort-by)
   (ellipsis :initarg :ellipsis :accessor pgmacstbl-ellipsis)
   (column-colors :initarg :column-colors :accessor pgmacstbl-column-colors)
   (row-colors :initarg :row-colors :accessor pgmacstbl-row-colors)
   (-cached-colors :initform nil)
   (-row-marks :initform nil)
   (-cache :initform (make-hash-table :test #'equal))
   (-cached-keymap :initform nil)
   (-has-column-spec :initform nil))
  "An object to hold the data for a table.")

(defvar-keymap pgmacstbl-map
  "S" #'pgmacstbl-sort-by-current-column
  "{" #'pgmacstbl-narrow-current-column
  "}" #'pgmacstbl-widen-current-column
  "g" #'pgmacstbl-revert-command
  "M-<left>" #'pgmacstbl-previous-column
  "M-<right>" #'pgmacstbl-next-column)

(defvar-keymap pgmacstbl-header-line-map
  :parent pgmacstbl-map
  "<follow-link>" 'mouse-face
  "<mouse-2>" #'pgmacstbl-header-line-sort)

(cl-defun make-pgmacstbl (&key columns objects objects-function
                            getter
                            formatter
                            displayer
                            (use-header-line t)
                            (face 'pgmacstbl)
                            actions keymap
                            (separator-width 1)
                            divider
                            divider-width
                            sort-by
                            (ellipsis t)
                            (insert t)
                            row-colors
                            column-colors)
  "Create and insert a pgmacstbl at point.
The pgmacstbl object is returned.  If INSERT is nil, the table won't
be inserted.

See info node `(pgmacstbl)Top' for pgmacstbl documentation."
  (when objects-function
    (setq objects (funcall objects-function)))
  ;; We'll be altering the list, so create a copy.
  (setq objects (copy-sequence objects))
  (let ((table
         (make-instance
          'pgmacstbl
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
    (setf (pgmacstbl-columns table)
          (mapcar (lambda (column)
                    (cond
                     ;; We just have the name (as a string).
                     ((stringp column)
                      (make-pgmacstbl-column :name column))
                     ;; A plist of keywords/values.
                     ((listp column)
                      (apply #'make-pgmacstbl-column column))
                     ;; A full `pgmacstbl-column' object.
                     (t
                      column)))
                  columns))
    ;; Compute missing column data.
    (setf (pgmacstbl-columns table) (pgmacstbl--compute-columns table))
    ;; Compute the row colors and marks. We are now ignoring column-colors.
    (when row-colors
      (let* ((size (if objects (length objects) 0))
             (colors (make-vector size nil))
             (row-marks (make-vector size nil))
             (row-faces (mapcar #'pgmacstbl--make-color-face row-colors))
             (row-face-count (length row-faces)))
        (dotimes (i size)
          (setf (aref colors i) (elt row-faces (mod i row-face-count))))
        (setf (slot-value table '-cached-colors) colors)
        (setf (slot-value table '-row-marks) row-marks)))
    ;; Compute the divider.
    (when (or divider divider-width)
      (setf (pgmacstbl-divider table)
            (propertize
             (or (copy-sequence divider)
                 (propertize
                  " " 'display
                  (list 'space :width
                        (list (pgmacstbl--compute-width table divider-width)))))
             'mouse-face 'highlight
             'keymap
             (define-keymap
               "<drag-mouse-1>" #'pgmacstbl--drag-resize-column
               "<down-mouse-1>" #'ignore))))
    ;; Compute the keymap.
    (setf (slot-value table '-cached-keymap) (pgmacstbl--make-keymap table))
    (unless sort-by
      (seq-do-indexed (lambda (column index)
                        (when (pgmacstbl-column-primary column)
                          (push (cons index (pgmacstbl-column-primary column))
                                (pgmacstbl-sort-by table))))
                      (pgmacstbl-columns table)))
    (when insert
      (pgmacstbl-insert table))
    table))

;; We need to account for the fact that the number of rows (of pgmacstbl-objects) may have changed,
;; so regenerate the '-cached-colors vector.
(defun pgmacstbl--update-colors (table)
  (let* ((rows (pgmacstbl-objects table))
         (size (if rows (length rows) 0))
         (colors (make-vector size nil))
         (row-faces (mapcar #'pgmacstbl--make-color-face (pgmacstbl-row-colors table)))
         (row-face-count (length row-faces)))
    (dotimes (i size)
      (setf (aref colors i) (elt row-faces (mod i row-face-count))))
    (setf (slot-value table '-cached-colors) colors)))

(defun pgmacstbl--compute-colors (row-colors column-colors)
  (cond
   ((null column-colors)
    (mapcar #'pgmacstbl--make-color-face row-colors))
   ((null row-colors)
    (mapcar #'pgmacstbl--make-color-face column-colors))
   (t
    (cl-loop for row in row-colors
             collect (cl-loop for column in column-colors
                              collect (pgmacstbl--face-blend
                                       (pgmacstbl--make-color-face row)
                                       (pgmacstbl--make-color-face column)))))))

(defun pgmacstbl--make-color-face (object)
  (if (stringp object)
      (list :background object)
    object))

(defun pgmacstbl--face-blend (face1 face2)
  (let ((foreground (pgmacstbl--face-color face1 face2 #'face-foreground
                                        :foreground))
        (background (pgmacstbl--face-color face1 face2 #'face-background
                                        :background)))
    `(,@(and foreground (list :foreground foreground))
      ,@(and background (list :background background)))))

(defun pgmacstbl--face-color (face1 face2 accessor slot)
  (let ((col1 (if (facep face1)
                  (funcall accessor face1)
                (plist-get face1 slot)))
        (col2 (if (facep face2)
                  (funcall accessor face2)
                (plist-get face2 slot))))
    (if (and col1 col2)
        (pgmacstbl--color-blend col1 col2)
      (or col1 col2))))

;;; FIXME: This is probably not the right way to blend two colors, is
;;; it?
(defun pgmacstbl--color-blend (color1 color2)
  (cl-destructuring-bind (r g b)
      (mapcar (lambda (n) (* (/ n 2) 255.0))
              (cl-mapcar #'+ (color-name-to-rgb color1)
                         (color-name-to-rgb color2)))
    (format "#%02X%02X%02X" r g b)))


(defun pgmacstbl-mark-row (table line-number mark)
  (let ((marks (slot-value table '-row-marks)))
    (setf (aref marks line-number) mark)))

(defun pgmacstbl-unmark-row (table line-number)
  (let ((marks (slot-value table '-row-marks)))
    (setf (aref marks line-number) nil)))

;; eg (pgmacstbl-register-mark-face :marked-for-deletion '(:foreground "red"))
(defun pgmacstbl-register-mark-face (mark face)
  (push (cons mark face) pgmacstbl--registered-mark-faces))


;;; Interface utility functions.

(defun pgmacstbl-current-table ()
  "Return the table under point."
  (get-text-property (point) 'pgmacstbl))

(defun pgmacstbl-current-object ()
  "Return the object under point."
  (get-text-property (point) 'pgmacstbl-object))

(defun pgmacstbl-current-column ()
  "Return the index of the column under point."
  (get-text-property (point) 'pgmacstbl-column))

(defun pgmacstbl-beginning-of-table ()
  "Go to the start of the current table."
  (goto-char (point-max))
  (if (text-property-search-backward 'pgmacstbl)
      (point)
    (goto-char (point-min))))

(defun pgmacstbl-end-of-table ()
  "Go to the end of the current table."
  (goto-char (point-min))
  (if (text-property-search-forward 'pgmacstbl)
      (point)
    (goto-char (point-max))))

(defun pgmacstbl-goto-object (object)
  "Go to OBJECT in the current table.
Return the position of the object if found, and nil if not."
  (let ((start (point)))
    (pgmacstbl-beginning-of-table)
    (save-restriction
      (narrow-to-region (point) (save-excursion (pgmacstbl-end-of-table)))
      (if (text-property-search-forward 'pgmacstbl-object object #'eq)
          (progn
            (forward-line -1)
            (point))
        (goto-char start)
        nil))))

(defun pgmacstbl-goto-table (table)
  "Go to TABLE in the current buffer.
If TABLE is found, return the position of the start of the table.
If it can't be found, return nil and don't move point."
  (let ((start (point)))
    (goto-char (point-min))
    (if-let* ((match (text-property-search-forward 'pgmacstbl table t)))
        (goto-char (prop-match-beginning match))
      (goto-char start)
      nil)))

(defun pgmacstbl-goto-column (column-number)
  "Go to COLUMN-NUMBER on the current line."
  (beginning-of-line)
  (if-let* ((match (text-property-search-forward 'pgmacstbl-column column-number t)))
      (let* ((tbl (pgmacstbl-current-table))
             (column (nth column-number (pgmacstbl-columns tbl))))
        (if (pgmacstbl-column--numerical column)
            (goto-char (- (prop-match-end match) 2))
          (goto-char (prop-match-beginning match))))
    (end-of-line)))

(cl-defun pgmacstbl-update-object (table object old-object)
  "Replace OLD-OBJECT in TABLE with OBJECT."
  (let* ((objects (pgmacstbl-objects table))
         (inhibit-read-only t))
    ;; First replace the object in the object storage.
    (if (eq old-object (car objects))
        ;; It's at the head, so replace it there.
        (setf (pgmacstbl-objects table)
              (cons object (cdr objects)))
      ;; Otherwise splice into the list.
      (while (and (cdr objects)
                  (not (eq (cadr objects) old-object)))
        (setq objects (cdr objects)))
      (unless objects
        (error "Can't find the old object"))
      (setcar (cdr objects) object))
    ;; Then update the cache.
    (unless (pgmacstbl--cache table)
      (cl-return-from pgmacstbl-update-object nil))
    (if-let* ((line-number (cl-position old-object (car (pgmacstbl--cache table))
                                        :key #'car
                                        :test #'eq))
              (line (elt (car (pgmacstbl--cache table)) line-number)))
        (progn
          (setcar line object)
          (setcdr line (pgmacstbl--compute-cached-line table object))
          ;; ... and redisplay the line in question.
          (save-excursion
            (pgmacstbl-goto-object old-object)
            (let ((keymap (get-text-property (point) 'keymap))
                  (start (point)))
              (delete-line)
              (pgmacstbl--insert-line table line line-number
                                   (nth 1 (pgmacstbl--cache table))
                                   (pgmacstbl--spacer table))
              (add-text-properties start (point) (list 'keymap keymap
                                                       'pgmacstbl table))))
          ;; We may have inserted a non-numerical value into a previously
          ;; all-numerical table, so recompute.
          (pgmacstbl--recompute-numerical table (cdr line)))
      (error "Can't find cached object in pgmacstbl"))))

(defun pgmacstbl-remove-object (table object)
  "Remove OBJECT from TABLE.
This will also remove the displayed line."
  ;; Update the marks vector (deleting the entry for this object).
  (let* ((deleted-line-number (cl-position object (pgmacstbl-objects table)))
         (current-row-marks (slot-value table '-row-marks))
         (updated-row-marks (make-vector (1- (length current-row-marks)) nil)))
    (cl-loop
     for i from 0 below deleted-line-number
     do (setf (aref updated-row-marks i) (aref current-row-marks i)))
    (cl-loop
     for i from (1+ deleted-line-number) below (length current-row-marks)
     do (setf (aref updated-row-marks (1- i)) (aref current-row-marks i)))
    (setf (slot-value table '-row-marks) updated-row-marks))
  ;; Remove object from the table objects.
  (setf (pgmacstbl-objects table) (delq object (pgmacstbl-objects table)))
  ;; Then adjust the cache and display.
  (let ((cache (pgmacstbl--cache table))
        (inhibit-read-only t))
    (when cache
      (setcar cache (delq (assq object (car cache)) (car cache))))
    (pgmacstbl--update-colors table)
    (save-excursion
      (pgmacstbl-goto-table table)
      (when (pgmacstbl-goto-object object)
        (delete-line))))
  ;; Redraw the table to fix the alternating row colors
  (pgmacstbl-revert))

(defun pgmacstbl-insert-object (table object &optional after-object)
  "Insert OBJECT into TABLE after AFTER-OBJECT.
If AFTER-OBJECT is nil (or doesn't exist in the table), insert
OBJECT at the end.
This also updates the displayed table."
  (let* ((current-row-marks (slot-value table '-row-marks))
         (updated-row-marks (make-vector (1+ (length current-row-marks)) nil))
         (new-line-number (if after-object
                              (1+ (cl-position after-object (pgmacstbl-objects table)))
                            (1- (length updated-row-marks)))))
    (cl-loop
     for i from 0 below new-line-number
     do (setf (aref updated-row-marks i) (aref current-row-marks i)))
    (cl-loop
     for i from (1+ new-line-number) below (length updated-row-marks)
     do (setf (aref updated-row-marks i) (aref current-row-marks (1- i))))
    (setf (slot-value table '-row-marks) updated-row-marks))
  ;; Insert into the objects.
  (let (pos)
    (if (and after-object
             (setq pos (memq after-object (pgmacstbl-objects table))))
        ;; Splice into list.
        (setcdr pos (cons object (cdr pos)))
      ;; Append.
      (nconc (pgmacstbl-objects table) (list object))))
  ;; Then adjust the cache and display.
  (save-excursion
    (pgmacstbl-goto-table table)
    (let* ((cache (pgmacstbl--ensure-cache table))
           (inhibit-read-only t)
           (keymap (get-text-property (point) 'keymap))
           (ellipsis (if (pgmacstbl-ellipsis table)
                         (propertize (truncate-string-ellipsis)
                                     'face (pgmacstbl-face table))
                       ""))
           (ellipsis-width (string-pixel-width ellipsis))
           (elem (and after-object
                      (assq after-object (car cache))))
           (line (cons object (pgmacstbl--compute-cached-line table object))))
      (if (not elem)
          ;; Append.
          (progn
            (setcar cache (nconc (car cache) (list line)))
            (pgmacstbl-end-of-table))
        ;; Splice into list.
        (let ((pos (memq elem (car cache))))
          (setcdr pos (cons line (cdr pos)))
          (unless (pgmacstbl-goto-object after-object)
            (pgmacstbl-end-of-table))))
      (pgmacstbl--update-colors table)
      (let ((start (point)))
        (pgmacstbl--insert-line table line 0
                             (nth 1 cache) (pgmacstbl--spacer table)
                             ellipsis ellipsis-width)
        (add-text-properties start (point) (list 'keymap keymap
                                                 'pgmacstbl table)))
      ;; We may have inserted a non-numerical value into a previously
      ;; all-numerical table, so recompute.
      (pgmacstbl--recompute-numerical table (cdr line)))))

(defun pgmacstbl-column (table index)
  "Return the name of the INDEXth column in TABLE."
  (pgmacstbl-column-name (elt (pgmacstbl-columns table) index)))

;;; Generating the table.

(defun pgmacstbl--get-value (object index column table)
  "Compute a cell value."
  (cond
   ((pgmacstbl-column-getter column)
    (funcall (pgmacstbl-column-getter column)
             object table))
   ((pgmacstbl-getter table)
    (funcall (pgmacstbl-getter table)
             object index table))
   ;; No getter functions; standard getters.
   ((stringp object)
    object)
   (t
    (elt object index))))

(defun pgmacstbl--compute-columns (table)
  (let ((numerical (make-vector (length (pgmacstbl-columns table)) t))
        (columns (pgmacstbl-columns table)))
    ;; First determine whether there are any all-numerical columns.
    (dolist (object (pgmacstbl-objects table))
      (seq-do-indexed
       (lambda (_elem index)
         (unless (numberp (pgmacstbl--get-value object index (elt columns index)
                                             table))
           (setf (elt numerical index) nil)))
       (pgmacstbl-columns table)))
    ;; Then fill in defaults.
    (seq-map-indexed
     (lambda (column index)
       ;; This is used when displaying.
       (unless (pgmacstbl-column-align column)
         (setf (pgmacstbl-column-align column)
               (if (elt numerical index)
                   'right
                 'left)))
       ;; This is used for sorting.
       (setf (pgmacstbl-column--numerical column)
             (elt numerical index))
       column)
     (pgmacstbl-columns table))))

(defun pgmacstbl--spacer (table)
  (pgmacstbl--compute-width table (pgmacstbl-separator-width table)))

(defun pgmacstbl--recompute-cache (table)
  (let* ((data (pgmacstbl--compute-cache table))
         (widths (pgmacstbl--compute-widths table data)))
    (setf (gethash (pgmacstbl--cache-key) (slot-value table '-cache))
          (list data widths))))

(defun pgmacstbl--ensure-cache (table)
  (or (pgmacstbl--cache table)
      (pgmacstbl--recompute-cache table)))

(defun pgmacstbl-insert (table)
  (let* ((spacer (pgmacstbl--spacer table))
         (start (point))
         (ellipsis (if (pgmacstbl-ellipsis table)
                       (propertize (truncate-string-ellipsis)
                                   'face (pgmacstbl-face table))
                     ""))
         (ellipsis-width (string-pixel-width ellipsis))
         ;; We maintain a cache per screen/window width, so that we render
         ;; correctly if Emacs is open on two different screens (or the
         ;; user resizes the frame).
         (widths (nth 1 (pgmacstbl--ensure-cache table))))
    ;; Don't insert any header or header line if the user hasn't
    ;; specified the columns.
    (when (slot-value table '-has-column-spec)
      (if (pgmacstbl-use-header-line table)
          (pgmacstbl--set-header-line table widths spacer)
        ;; Insert the header line directly into the buffer, and put a
        ;; keymap to be able to sort the columns there (by clicking on
        ;; them).
        (pgmacstbl--insert-header-line table widths spacer)
        (add-text-properties start (point)
                             (list 'keymap pgmacstbl-header-line-map
                                   'rear-nonsticky t
                                   'pgmacstbl table))
        (setq start (point))))
    (pgmacstbl--sort table)
    ;; Insert the data.
    (let ((line-number 0))
      (dolist (line (car (pgmacstbl--cache table)))
        (pgmacstbl--insert-line table line line-number widths spacer
                             ellipsis ellipsis-width)
        (setq line-number (1+ line-number))))
    (add-text-properties start (point)
                         (list 'rear-nonsticky t
                               'pgmacstbl table))
    (goto-char start)))

(defun pgmacstbl--insert-line (table line line-number widths spacer
                                  &optional ellipsis ellipsis-width)
  (let ((start (point))
        (columns (pgmacstbl-columns table))
        (divider (pgmacstbl-divider table))
        (keymap (slot-value table '-cached-keymap)))
    (seq-do-indexed
     (lambda (elem index)
       (let ((value (nth 0 elem))
             (column (elt columns index))
             (pre-computed (nth 2 elem)))
         ;; See if we have any formatters here.
         (cond
          ((pgmacstbl-column-formatter column)
           (setq value (funcall (pgmacstbl-column-formatter column) value)
                 pre-computed nil))
          ((pgmacstbl-formatter table)
           (setq value (funcall (pgmacstbl-formatter table)
                                value index table)
                 pre-computed nil)))
         (let ((displayed
                ;; Allow any displayers to have their say.
                (cond
                 ((pgmacstbl-column-displayer column)
                  (funcall (pgmacstbl-column-displayer column)
                           value (elt widths index) table))
                 ((pgmacstbl-displayer table)
                  (funcall (pgmacstbl-displayer table)
                           value index (elt widths index) table))
                 (pre-computed
                  ;; If we don't have a displayer, use the pre-made
                  ;; (cached) string value.
                  (if (> (nth 1 elem) (elt widths index))
                      (concat
                       (pgmacstbl--limit-string
                        pre-computed (- (elt widths index)
                                        (or ellipsis-width 0)))
                       ellipsis)
                    pre-computed))
                 ;; Recompute widths.
                 (t
                  (if (> (string-pixel-width value) (elt widths index))
                      (concat
                       (pgmacstbl--limit-string
                        value (- (elt widths index)
                                 (or ellipsis-width 0)))
                       ellipsis)
                    value))))
               (start (point))
               ;; Don't insert the separator after the final column.
               (last (= index (- (length line) 2))))
           (if (eq (pgmacstbl-column-align column) 'left)
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
           (put-text-property start (point) 'pgmacstbl-column index)
           (put-text-property start (point) 'keymap keymap)
           (when divider
             (insert divider)
             (setq start (point))))))
     (cdr line))
    (insert "\n")
    (put-text-property start (point) 'pgmacstbl-object (car line))
    (put-text-property start (point) 'pgmacstbl-line-number line-number)
    (let* ((colors (slot-value table '-cached-colors))
           (row-mark (aref (slot-value table '-row-marks) line-number))
           (extra-mark-face (cdr (assoc row-mark pgmacstbl--registered-mark-faces))))
      (add-face-text-property start (point) (aref colors line-number))
      (when extra-mark-face
        ;; We add this face characteristic (which is probably only something like a different
        ;; background-color) on top of the existing face characteristics.
        (add-face-text-property start (point) extra-mark-face)))))

(defun pgmacstbl--cache-key ()
  (cons (frame-terminal) (window-width)))

(defun pgmacstbl--cache (table)
  (gethash (pgmacstbl--cache-key) (slot-value table '-cache)))

(defun pgmacstbl--clear-cache (table)
  (setf (gethash (pgmacstbl--cache-key) (slot-value table '-cache)) nil))

(defun pgmacstbl--sort (table)
  (pcase-dolist (`(,index . ,direction) (pgmacstbl-sort-by table))
    (let ((cache (pgmacstbl--cache table))
          (numerical (pgmacstbl-column--numerical
                      (elt (pgmacstbl-columns table) index)))
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

(defun pgmacstbl--indicator (table index)
  (let ((order (car (last (pgmacstbl-sort-by table)))))
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

(defun pgmacstbl--insert-header-line (table widths spacer)
  (cl-flet ((space-for (width)
              (propertize " " 'display (list 'space :width (list width)))))
    (let ((start (point))
          (divider (pgmacstbl-divider table))
          (cmap (define-keymap
                  "<header-line> <drag-mouse-1>" #'pgmacstbl--drag-resize-column
                  "<header-line> <down-mouse-1>" #'ignore))
          (dmap (define-keymap
                  "<header-line> <drag-mouse-1>"
                  (lambda (e)
                    (interactive "e")
                    (pgmacstbl--drag-resize-column e t))
                  "<header-line> <down-mouse-1>" #'ignore)))
      (seq-do-indexed
       (lambda (column index)
         (let* ((name (propertize
                       (pgmacstbl-column-name column)
                       'face (list 'pgmacs-table-header)
                       'mouse-face 'header-line-highlight
                       'keymap cmap))
                (start (point))
                (indicator (pgmacstbl--indicator table index))
                (indicator-width (string-pixel-width indicator))
                (last (= index (1- (length (pgmacstbl-columns table)))))
                (displayed (if (> (string-pixel-width name)
                                  (- (elt widths index) indicator-width))
                               (pgmacstbl--limit-string
                                name (- (elt widths index) indicator-width))
                             name)))
           (let ((fill-width (- (elt widths index)
                                (string-pixel-width displayed)
                                indicator-width)))
             (if (eq (pgmacstbl-column-align column) 'left)
                 (insert displayed (space-for fill-width) indicator)
               (insert (space-for fill-width) displayed indicator)))
           (unless last
             (insert (space-for spacer)))
           (when (and divider (not last))
             (insert (propertize divider 'keymap dmap)))
           (put-text-property start (point) 'pgmacstbl-column index)))
       (pgmacstbl-columns table))
      (insert "\n")
      (add-face-text-property start (point) 'header-line))))

(defun pgmacstbl--drag-resize-column (e &optional next)
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
			            'pgmacstbl-column
			            (car obj))
                 ;; For reasons of efficiency, we don't have that in
                 ;; the buffer itself, so find the column.
                 (save-excursion
                   (goto-char (posn-point pos-start))
                   (1+
                    (get-text-property
                     (prop-match-beginning
                      (text-property-search-backward 'pgmacstbl-column))
                     'pgmacstbl-column)))))
            (start-x (car (posn-x-y pos-start)))
            (end-x (car (posn-x-y (event-end e)))))
        (when (or (> column 0) next)
          (pgmacstbl--alter-column-width (pgmacstbl-current-table)
                                      (if next
                                          column
                                        (1- column))
                                      (- end-x start-x)))))))

(defun pgmacstbl--recompute-numerical (table line)
  "Recompute numericalness of columns if necessary."
  (let ((columns (pgmacstbl-columns table))
        (recompute nil))
    (seq-do-indexed
     (lambda (elem index)
       (when (and (pgmacstbl-column--numerical (elt columns index))
                  (not (numberp elem)))
         (setq recompute t)))
     line)
    (when recompute
      (pgmacstbl--compute-columns table))))

(defun pgmacstbl--set-header-line (table widths spacer)
  (setq header-line-format
        (string-replace
         "%" "%%"
         (with-temp-buffer
           (insert " ")
           (pgmacstbl--insert-header-line table widths spacer)
           ;; Align the header with the (possibly) fringed buffer text.
           (put-text-property
            (point-min) (1+ (point-min))
            'display '(space :align-to 0))
           (buffer-substring (point-min) (1- (point-max))))))
  (pgmacstbl-header-mode 1))

(defun pgmacstbl--limit-string (string pixels)
  (while (and (length> string 0)
              (> (string-pixel-width string) pixels))
    (setq string (substring string 0 (1- (length string)))))
  string)

(defun pgmacstbl--char-width (table)
  (string-pixel-width (propertize "x" 'face (pgmacstbl-face table))))

(defun pgmacstbl--compute-width (table spec)
  (cond
   ((numberp spec)
    (* spec (pgmacstbl--char-width table)))
   ((string-match "\\([0-9.]+\\)ex" spec)
    (* (string-to-number (match-string 1 spec)) (pgmacstbl--char-width table)))
   ((string-match "\\([0-9.]+\\)px" spec)
    (string-to-number (match-string 1 spec)))
   ((string-match "\\([0-9.]+\\)%" spec)
    (/ (* (string-to-number (match-string 1 spec)) (window-width nil t))
       100))
   (t
    (error "Invalid spec: %s" spec))))

(defun pgmacstbl--compute-widths (table cache)
  "Compute the display widths for TABLE."
  (seq-into
   (seq-map-indexed
    (lambda (column index)
      (let ((width
             (or
              ;; Explicit widths.
              (and (pgmacstbl-column-width column)
                   (pgmacstbl--compute-width table (pgmacstbl-column-width column)))
              ;; Compute based on the displayed widths of
              ;; the data.
              (seq-max (seq-map (lambda (elem)
                                  (nth 1 (elt (cdr elem) index)))
                                cache)))))
        ;; Let min-width/max-width specs have their say.
        (when-let* ((min-width (and (pgmacstbl-column-min-width column)
                                   (pgmacstbl--compute-width
                                    table (pgmacstbl-column-min-width column)))))
          (setq width (max width min-width)))
        (when-let* ((max-width (and (pgmacstbl-column-max-width column)
                                   (pgmacstbl--compute-width
                                    table (pgmacstbl-column-max-width column)))))
          (setq width (min width max-width)))
        width))
    (pgmacstbl-columns table))
   'vector))

(defun pgmacstbl--compute-cache (table)
  (seq-map
   (lambda (object)
     (cons object (pgmacstbl--compute-cached-line table object)))
   (pgmacstbl-objects table)))

(defun pgmacstbl--compute-cached-line (table object)
  (seq-map-indexed
   (lambda (column index)
     (let* ((value (pgmacstbl--get-value object index column table))
            (string (if (stringp value)
                        (copy-sequence value)
                      (format "%s" value))))
       (add-face-text-property 0 (length string)
                               (pgmacstbl-face table)
                               t string)
       ;; We stash the computed width and string here -- if there are
       ;; no formatters/displayers, we'll be using the string, and
       ;; then won't have to recreate it.
       (list value (string-pixel-width string) string)))
   (pgmacstbl-columns table)))

(defun pgmacstbl--make-keymap (table)
  (let ((map (if (or (pgmacstbl-actions table)
                     (pgmacstbl-keymap table))
                 (copy-keymap pgmacstbl-map)
               pgmacstbl-map)))
    (when-let* ((actions (pgmacstbl-actions table)))
      (while actions
        (funcall (lambda (key binding)
                   (keymap-set map key
                               (lambda (object)
                                 (interactive (list (pgmacstbl-current-object)))
                                 (funcall binding object))))
                 (car actions) (cadr actions))
        (setq actions (cddr actions))))
    (if (pgmacstbl-keymap table)
        (progn
          (setf (pgmacstbl-keymap table)
                (copy-keymap (pgmacstbl-keymap table)))
          ;; Respect any previously set parent keymaps.
          (set-keymap-parent (pgmacstbl-keymap table)
                             (if (keymap-parent (pgmacstbl-keymap table))
                                 (append (ensure-list
                                          (pgmacstbl-keymap table))
                                         (list map))
                               map))
          (pgmacstbl-keymap table))
      map)))

(defun pgmacstbl-revert ()
  "Regenerate the table under point."
  (let ((table (pgmacstbl-current-table))
        (object (pgmacstbl-current-object))
        (column (pgmacstbl-current-column))
        (inhibit-read-only t))
    (unless table
      (user-error "No table under point"))
    (delete-region (pgmacstbl-beginning-of-table) (pgmacstbl-end-of-table))
    (pgmacstbl-insert table)
    (when object
      (pgmacstbl-goto-object object))
    (when column
      (pgmacstbl-goto-column column))))

(defun pgmacstbl--widths (table)
  (nth 1 (pgmacstbl--ensure-cache table)))

;;; Commands.

(defvar-keymap pgmacstbl-header-mode-map
  "<header-line> <mouse-1>" 'pgmacstbl-header-line-sort
  "<header-line> <mouse-2>" 'pgmacstbl-header-line-sort)

(define-minor-mode pgmacstbl-header-mode
  "Minor mode for buffers with pgmacstbls with headers."
  :keymap pgmacstbl-header-mode-map)

(defun pgmacstbl-narrow-current-column (&optional n)
  "Narrow the current column by N characters.
If N isn't given, N defaults to 1.

Interactively, N is the prefix argument."
  (interactive "p")
  (let* ((table (pgmacstbl-current-table))
         (column (pgmacstbl-current-column)))
    (unless column
      (user-error "No column under point"))
    (pgmacstbl--alter-column-width table column
                                (- (* (pgmacstbl--char-width table) (or n 1))))))

(defun pgmacstbl--alter-column-width (table column delta)
  (let ((widths (pgmacstbl--widths table)))
    (setf (aref widths column)
          (max (* (pgmacstbl--char-width table) 2)
               (+ (aref widths column) delta)))
    ;; Store the width so it'll be respected on a revert.
    (setf (pgmacstbl-column-width (elt (pgmacstbl-columns table) column))
          (format "%dpx" (aref widths column)))
    (pgmacstbl-revert)))

(defun pgmacstbl-widen-current-column (&optional n)
  "Widen the current column by N characters.
If N isn't given, N defaults to 1.

Interactively, N is the prefix argument."
  (interactive "p")
  (pgmacstbl-narrow-current-column (- n)))

(defun pgmacstbl-previous-column ()
  "Go to the previous column."
  (interactive)
  (pgmacstbl-goto-column
   (max 0 (1- (or (pgmacstbl-current-column)
                  (length (pgmacstbl--widths (pgmacstbl-current-table))))))))

(defun pgmacstbl-next-column ()
  "Go to the next column."
  (interactive)
  (when (pgmacstbl-current-column)
    (pgmacstbl-goto-column
     (min (1- (length (pgmacstbl--widths (pgmacstbl-current-table))))
          (1+ (pgmacstbl-current-column))))))

(defun pgmacstbl-revert-command ()
  "Re-query data and regenerate the table under point."
  (interactive)
  (let ((table (pgmacstbl-current-table)))
    (when (pgmacstbl-objects-function table)
      (setf (pgmacstbl-objects table) (funcall (pgmacstbl-objects-function table))))
    (pgmacstbl--clear-cache table)
    (pgmacstbl--ensure-cache table))
  (pgmacstbl-revert))

(defun pgmacstbl-sort-by-current-column ()
  "Sort the table under point by the column under point."
  (interactive)
  (unless (pgmacstbl-current-column)
    (user-error "No current column"))
  (let* ((table (pgmacstbl-current-table))
         (last (car (last (pgmacstbl-sort-by table))))
         (index (pgmacstbl-current-column)))
    ;; First prune any previous appearance of this column.
    (setf (pgmacstbl-sort-by table)
          (delq (assq index (pgmacstbl-sort-by table))
                (pgmacstbl-sort-by table)))
    ;; Then insert this as the last sort key.
    (setf (pgmacstbl-sort-by table)
          (append (pgmacstbl-sort-by table)
                  (list (cons index
                              (if (eq (car last) index)
                                  (if (eq (cdr last) 'ascend)
                                      'descend
                                    'ascend)
                                'ascend))))))
  (pgmacstbl-revert))

(defun pgmacstbl-header-line-sort (e)
  "Sort a pgmacstbl from the header line."
  (interactive "e")
  (let* ((pos (event-start e))
	 (obj (posn-object pos)))
    (with-current-buffer (window-buffer (posn-window pos))
      (goto-char (point-min))
      (pgmacstbl-goto-column
       (get-text-property (if obj (cdr obj) (posn-point pos))
			  'pgmacstbl-column
			  (car obj)))
      (pgmacstbl-sort-by-current-column))))

(provide 'pgmacstbl)

;;; pgmacstbl.el ends here
