;;; test.el --- PGmacs test entrypoints  -*- lexical-binding: t; -*-

(require 'keycast)
(require 'cl-lib)

;; Also tried showkey.el with (showkey-tooltip-mode) but doesn't seem to work on Wayland?



(defun pgmacs-open-pguri ()
  (interactive)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (keycast-mode-line-mode 1)
  ;; Only display Control / Meta / etc keys
  (cl-pushnew (list 'self-insert-command nil nil) keycast-substitute-alist)
  (setq debug-on-error t)
  (setq pgmacs-enable-query-logging t)
  (pgmacs-open-uri (or (getenv "PGURI")
                       "postgresql://pgeltestuser:pgeltest@127.0.0.1/pgeltestdb")))


(defun pgmacs-test ()
  (interactive)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (keycast-mode-line-mode 1)
  ;; Only display Control / Meta / etc keys
  (cl-pushnew (list 'self-insert-command nil nil) keycast-substitute-alist)
  (setq debug-on-error t)
  (setq pgmacs-enable-query-logging t)
  (let* ((*pg* (if (or (string-match "WSL" operating-system-release)
                       (eq system-type 'windows-nt)
                       (eq system-type 'ms-dos))
                   (pg-connect "pgeltestdb" "pgeltestuser" "pgeltest")
                 (pg-connect-local "/var/run/postgresql/.s.PGSQL.5432" "pgeltestdb" "pgeltestuser" "pgeltest"))))
    (pgmacs-open *pg*)))


;; Test the normal widget-based entry to PGmacs
(defun pgmacs-widget ()
  (interactive)
  (setq pgmacs-enable-query-logging t)
  (pgmacs))


(defun pgmacs-test-minimal ()
  (interactive)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (setq debug-on-error t)
  (setq pgmacs-enable-query-logging t)
  (let* ((*pg* (pg-connect-local "/var/run/postgresql/.s.PGSQL.5432" "minimal" "pgeltestuser" "pgeltest")))
    (pgmacs-open *pg*)))


(defun pgmacs-test-pagila ()
  (interactive)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (keycast-mode-line-mode 1)
  ;; Only display Control / Meta / etc keys
  (cl-pushnew (list 'self-insert-command nil nil) keycast-substitute-alist)
  (setq debug-on-error t)
  (let* ((*pg* (if (or (string-match "WSL" operating-system-release)
                       (eq system-type 'windows-nt)
                       (eq system-type 'ms-dos))
                   (pg-connect "pagila" "pgeltestuser" "pgeltest")
                 (pg-connect-local "/var/run/postgresql/.s.PGSQL.5432" "pagila" "pgeltestuser" "pgeltest"))))
    (pgmacs-open *pg*)))


(defun pgmacs-test-shakespeare ()
  (interactive)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (keycast-mode-line-mode 1)
  ;; Only display Control / Meta / etc keys
  (cl-pushnew (list 'self-insert-command nil nil) keycast-substitute-alist)
  (setq debug-on-error t)
  (let* ((*pg* (if (or (string-match "WSL" operating-system-release)
                       (eq system-type 'windows-nt)
                       (eq system-type 'ms-dos))
                   (pg-connect "shakespeare" "pgeltestuser" "pgeltest")
                 (pg-connect-local "/var/run/postgresql/.s.PGSQL.5432" "shakespeare" "pgeltestuser" "pgeltest"))))
    (pgmacs-open *pg*)))


;; This is a good test case for SchemaSpy support
(defun pgmacs-test-chinook ()
  (interactive)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (keycast-mode-line-mode 1)
  ;; Only display Control / Meta / etc keys
  (cl-pushnew (list 'self-insert-command nil nil) keycast-substitute-alist)
  (setq debug-on-error t)
  (let* ((*pg* (if (or (string-match "WSL" operating-system-release)
                       (eq system-type 'windows-nt)
                       (eq system-type 'ms-dos))
                   (pg-connect "chinook" "pgeltestuser" "pgeltest")
                 (pg-connect-local "/var/run/postgresql/.s.PGSQL.5432" "chinook" "pgeltestuser" "pgeltest"))))
    (pgmacs-open *pg*)))



;; The postgres user can connect locally without a password; this is a test for auth without a password.
(defun pgmacs-test-postgres ()
  (interactive)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (setq debug-on-error t)
  (let* ((*pg* (if (or (string-match "WSL" operating-system-release)
                       (eq system-type 'windows-nt)
                       (eq system-type 'ms-dos))
                   (pg-connect "template1" "postgresl" "ignored")
                 (pg-connect-local "/var/run/postgresql/.s.PGSQL.5432" "template1" "postgres" "ignored"))))
    (pgmacs-open *pg*)))


;; This is a PostgreSQL server that is open to the public (but fantastically slow...)
(defun pgmacs-test-ebiacuk ()
  (interactive)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  ;; (setq debug-on-error t)
  (pgmacs-open-uri "postgres://reader:NWDMCE5xdipIjRrp@hh-pgsql-public.ebi.ac.uk:5432/pfmegrnargs"))


