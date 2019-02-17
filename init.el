;;; one file config

(defconst EMACS26+
  (eval-when-compile (not (version< emacs-version "26"))))
(defconst EMACS27+
  (eval-when-compile (not (version< emacs-version "27"))))

(defconst IS-MAC     (eq system-type 'darwin))
(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WIN (memq system-type '(cygwin windows-nt ms-dos)))
(defconst sea-emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to this emacs.d directory. Must end in a slash.")
(defconst sea-local-dir (concat sea-emacs-dir ".local/")
  "Root directory for local Emacs files. Use this as permanent storage for files
that are safe to share across systems (if this config is symlinked across
several computers).")

(defconst sea-etc-dir (concat sea-local-dir "etc/")
  "Directory for non-volatile storage.

Use this for files that don't change much, like servers binaries, external
dependencies or long-term shared data.")

(defconst sea-cache-dir (concat sea-local-dir "cache/")
  "Directory for volatile storage.

Use this for files that change often, like cache files.")

(defconst sea-gc-max (eval-when-compile (* 500 1024 1024)))
(defconst sea-gc-min (eval-when-compile (* 200 1024 1024)))
(defun sea-minibuffer-setup-hook nil (setq gc-cons-threshold sea-gc-max))
(defun sea-minibuffer-exit-hook nil (setq gc-cons-threshold sea-gc-min))
(add-hook 'minibuffer-setup-hook #'sea-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'sea-minibuffer-exit-hook)

(require 'package)
(setq package-enable-at-startup nil
      package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
			 ("melpa" . "http://elpa.emacs-china.org/melpa-stable/")
			 ("melpa-stable" . "http://elpa.emacs-china.org/melpa/")
			 ("org" . "http://elpa.emacs-china.org/org/")
			 ("sunrise-commander" . "http://elpa.emacs-china.org/sunrise-commander/")
			 ("user42" . "http://elpa.emacs-china.org/user42/")
			 ))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(unless (package-installed-p 'quelpa)
  (package-refresh-contents)
  (package-install 'quelpa))
(setq quelpa-update-melpa-p nil)
(require 'quelpa)
(setq use-package-always-ensure t)
;; (setq use-package-ensure-function 'quelpa)
(setq use-package-always-defer t)
(use-package diminish)
(use-package general)

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://framagit.org/steckerhalter/quelpa-use-package.git"))

(require 'quelpa-use-package)
; (use-package quelpa-use-package
    ; :ensure t
    ; :init
    ; (setq quelpa-update-melpa-p nil))



(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
(require 'init-basic)
(require 'init-locales)
(require 'init-defun)
(require 'init-ui)
(require 'init-treemacs)
;(require 'init-neotree)
;(require 'init-modeline)
(require 'init-highlight)
(require 'init-ivy)
(require 'init-company)
(require 'init-projectile)
(require 'init-util)
(require 'init-lsp)
(require 'init-rust)
(require 'init-go)


;; Start server
(require 'server)
(unless (server-running-p)
  (server-start))

(dolist (dir (list sea-cache-dir sea-etc-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; Variables configured via the interactive 'customize' interface
(setq custom-file (concat sea-cache-dir "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))


;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
