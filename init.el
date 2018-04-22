;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq gc-cons-threshold 30000000)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after init"
            (setq file-name-handler-alist default-file-name-handler-alist)
            (setq gc-cons-threshold 800000)))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
;; Packages
(require 'init-package)
(require 'init-basic)
(require 'init-funcs)
(require 'init-dired)
(require 'init-restore)
(require 'init-ivy)
(require 'init-projectile)
(require 'init-highlight)
(require 'init-edit)
(require 'init-window)
(require 'init-company)
(require 'init-org)
(require 'init-utils)
;; programing config
(require 'init-prog)
(require 'init-emacs-lisp)
(require 'init-py)
(put 'dired-find-alternate-file 'disabled nil)
