;;; Load the pg-el library from MELPA.  -*- lexical-binding: t; -*-
;;
;; Needed when running tests with "emacs -Q".

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-install 'pg)
(package-install 'keycast)
