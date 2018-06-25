;;; one file config

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;; Standard package repositories
(require 'package)
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
                         ("org" . "http://elpa.emacs-china.org/org/")))
(package-initialize)
(defun config-proxy()
  "config proxy when you are using proxy to access internet"
  (setq url-proxy-services '(("http" . "127.0.0.1:3128") ;
                             ("https" . "127.0.0.1:3128")))
  (setq url-http-proxy-basic-auth-storage
        (list (list "127.0.0.1:3128"
                    (cons "Input your LDAP UID !"
                          (base64-encode-string "w012345:123456"))))))
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            ;; Record this as a package the user installed explicitly
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(require-package 'use-package)
(require-package 'wgrep)
(require-package 'diminish)
(require-package 'scratch)
(require-package 'command-log-mode)
(require-package 'cl-lib)
(require 'cl-lib)

(require 'server)
(unless (server-running-p)
  (server-start))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
