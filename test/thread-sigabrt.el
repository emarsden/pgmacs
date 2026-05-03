;;; thread-freeze.el   -*- lexical-binding: t; -*-
;;
;; Copyright: (C) 2026  Eric Marsden
;; Author: Eric Marsden <eric.marsden@risk-engineering.org>

;; To run:
;;
;; Set up a local echo server on port 12345 with for example
;;
;;    podman run -p 127.0.0.1:12345:12345 docker.io/alpine sh -c "apk add --no-cache socat && socat TCP-LISTEN:12345,fork EXEC:/bin/cat"
;;
;; then load this file into Emacs
;;
;;    emacs -Q -l thread-sigabrt.el -f run


(require 'cl-lib)
(require 'eieio)
(require 'text-property-search)


(defvar-local bug--con nil)

(defvar bug-use-worker-thread t)

(cl-defstruct bug--worker
  (con nil)
  (thread nil)
  (tasks-mutex (make-mutex "Bug-worker"))
  (tasks-list (list))
  (tasks-ready nil))

;; State associated with a background worker thread, a bug--worker instance. This is a
;; buffer-local variable, like bug--con, in order to allow different active buffers connected to
;; different PostgreSQL instances. When bug-use-worker-thread is enabled, each table-list buffer
;; is associated with one worker connection, and one background worker thread.
(defvar-local bug--worker-state nil)

;; A function to call on thread startup for initialization. This will typically contain code to set
;; up our connection to PostgreSQL, which needs to be established from the new thread in order for
;; the thread to be able to accept-process-output from the connection.
(defvar bug--worker-initializer nil)

;; The retriever function makes database queries and returns a calculated-value. The inserter
;; function takes the calculated-value and inserts it (rapidly, in blocking mode) at the location
;; pointed to by marker.
(cl-defstruct bug--task
  retriever
  inserter
  marker)

(defun bug--worker-add-task (retriever inserter marker)
  (with-slots (con tasks-mutex tasks-list) bug--worker-state
    (let ((task (make-bug--task :retriever retriever
                                   :inserter inserter
                                   :marker marker)))
      (with-mutex tasks-mutex
        (push task tasks-list)))))

(defun bug--worker-pop-task ()
  (unless bug--worker-state
    (error "Buffer-local bug--worker-state variable is not set"))
  (with-slots (tasks-mutex tasks-list) bug--worker-state
    (with-mutex tasks-mutex
      (pop tasks-list))))

(defun bug--worker-tasks-reset ()
  (unless bug--worker-state
    (error "Buffer-local bug--worker-state variable is not set"))
  (with-slots (tasks-mutex tasks-list tasks-ready) bug--worker-state
    (setq tasks-ready nil)
    (with-mutex tasks-mutex
      (setq tasks-list (list)))))

(defun bug--worker-tasks-start ()
  (unless bug--worker-state
    (error "Buffer-local bug--worker-state variable is not set"))
  ;; Make sure that the tasks are executed in the same order as they were added to the task list, so
  ;; that the marker positions are not disturbed by the inserted text.
  (with-slots (tasks-mutex tasks-list tasks-ready) bug--worker-state
    (with-mutex tasks-mutex
      (setq tasks-list (nreverse tasks-list)))
    (setq tasks-ready t))
  ;; If not using a worker thread, run all the pending tasks now, synchronously.
  (unless bug-use-worker-thread
    (cl-loop
     for task = (bug--worker-pop-task)
     while task
     do (with-slots (retriever inserter marker) task
          (with-current-buffer (marker-buffer marker)
            (let ((calculated (funcall retriever bug--con))
                  (buffer-read-only nil))
              (save-excursion
                (goto-char (marker-position marker))
                (funcall inserter calculated)
                ;; Delete the placeholder text.
                (goto-char (marker-position marker))
                (when-let* ((match (text-property-search-backward 'bug--placeholder nil nil)))
                  (delete-region (prop-match-beginning match) (prop-match-end match))))))))))

(defun bug--worker-runner ()
  "The function run in a background worker thread."
  (unless bug--worker-state
    (error "Buffer-local bug--worker-state variable is not set"))
  (when (eq main-thread (current-thread))
    (error "Don't run worker on the main thread"))
  (message "Entered the worker thread, running initializer")
  (when bug--worker-initializer
    (funcall bug--worker-initializer))
  (with-slots (con tasks-ready) bug--worker-state
    (while t
      (thread-yield)
      (sit-for 0.5)
      (while (not tasks-ready)
        (thread-yield)
        (sit-for 1))
      (condition-case e
          (when-let* ((task (bug--worker-pop-task)))
            (with-slots (retriever inserter marker) task
              (with-current-buffer (marker-buffer marker)
                (let ((calculated (funcall retriever con))
                      (buffer-read-only nil))
                  (save-excursion
                    (goto-char (marker-position marker))
                    (funcall inserter calculated)
                    ;; delete the placeholder text
                    (goto-char (marker-position marker))
                    (when-let* ((match (text-property-search-backward 'bug--placeholder nil nil)))
                      (delete-region (prop-match-beginning match) (prop-match-end match))))))))
        (user-error (message "worker thread user-error %s" e))
        (error (message "worker thread error %s" e))))))

(defun bug--fetch (con msg)
  (with-current-buffer (process-buffer con)
    (erase-buffer)
    (process-send-string con msg)
    (accept-process-output)
    (sleep-for 3)
    (buffer-substring (point-min) (point-max))))

(defun run ()
  (setq bug--worker-initializer
        (lambda ()
          (message "In worker initializer")
          (let* ((buf (get-buffer-create "*bug echo*"))
                 (wcon (make-network-process :name "echo service"
                                             :buffer buf
                                             :host "127.0.0.1"
                                             :service 12345
                                             :coding nil)))
            (setf (bug--worker-con bug--worker-state) wcon))))
  (switch-to-buffer (get-buffer-create "*bug*"))
  (setq bug--worker-state (make-bug--worker))
  (setf (bug--worker-thread bug--worker-state) (make-thread 'bug--worker-runner "worker thread"))
  (with-slots (con thread) bug--worker-state
    (dotimes (i 1000)
      (sit-for 0.5)
      (bug--fetch con (format "twiddle%s" i))
      (insert i " ")
      (when (thread-live-p thread)
        (thread-signal thread 'user-error (list "foo"))))))


;; EOF
