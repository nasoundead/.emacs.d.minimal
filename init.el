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

(require 'package)
(setq package-enable-at-startup nil
      package-archives '(("gnu" . "http://elpa.emacs-china.org/gnu/")
			 ("melpa" . "http://elpa.emacs-china.org/melpa/")
			 ("org" . "http://elpa.emacs-china.org/org/")
			 ("sunrise-commander" . "http://elpa.emacs-china.org/sunrise-commander/")
			 ("user42" . "http://elpa.emacs-china.org/user42/")
			 ))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(use-package diminish)
(use-package general)

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(require 'init-basic)
(require 'init-ui)
(require 'init-ivy)
(require 'init-company)


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
