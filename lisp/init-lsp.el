;; init-lsp.el --- Initialize lsp configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 Haibo Wang

;; Author: Bruce Wong <nasoundead@163.com>
;; URL: https://github.com/nasoundead/.emacs.d.minimal

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Golang configurations.
;;

;;; Code:
;;
;;

(use-package lsp-mode
  :ensure t
  :hook (prog-mode . lsp)
  :bind (:map lsp-mode-map
	      ("C-c C-d" . lsp-describe-thing-at-point))
  :init (setq lsp-prefer-flymake nil
	      lsp-auto-guess-root t
	      flymake-fringe-indicator-position 'right-fringe)
  )
(use-package company-lsp
  :init (setq company-lsp-cache-candidates 'auto))

(use-package lsp-ui
  :ensure t
  :custom-face
  (lsp-ui-doc-background ((t (:background ,(face-background 'tooltip)))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind (:map lsp-ui-mode-map
	      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	      ([remap xref-find-references] . lsp-ui-peek-find-references)
	      ("C-c u" . lsp-ui-imenu))
  :init
  (setq lsp-ui-doc-enable t
	lsp-ui-doc-include-signature t
	lsp-ui-doc-position 'at-point
	lsp-ui-doc-use-webkit t
	lsp-ui-doc-delay 1.0
	lsp-ui-doc-border (face-foreground 'default)
	lsp-ui-sideline-enable nil
	lsp-ui-sideline-ignore-duplicate t)
  :config
  (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)
  ;; Reset `lsp-ui-doc-background' after loading theme
  (add-hook 'after-load-theme-hook
	    (lambda ()
	      (setq lsp-ui-doc-border (face-foreground 'default))
	      (set-face-background 'lsp-ui-doc-background
				   (face-background 'tooltip))))
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil))
  )

(use-package lsp-java :ensure t :after lsp
  :config (add-hook 'java-mode-hook 'lsp))


;; Debug
(use-package dap-mode
  :diminish
  :functions dap-hydra/nil
  :bind (:map lsp-mode-map
	      ("<f5>" . dap-debug)
	      ("M-<f5>" . dap-hydra))
  :hook ((after-init . dap-mode)
	 (dap-mode . dap-ui-mode)
	 (dap-session-created . (lambda (&_rest) (dap-hydra)))
	 (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))

	 (python-mode . (lambda () (require 'dap-python)))
	 (ruby-mode . (lambda () (require 'dap-ruby)))
	 (go-mode . (lambda () (require 'dap-go)))
	 (java-mode . (lambda () (require 'dap-java)))
	 ((c-mode c++-mode objc-mode swift) . (lambda () (require 'dap-lldb)))
	 (php-mode . (lambda () (require 'dap-php)))
	 (elixir-mode . (lambda () (require 'dap-elixir)))
	 ((js-mode js2-mode) . (lambda () (require 'dap-chrome)))))

(use-package lsp-treemacs
  :bind (:map lsp-mode-map
	      ("M-9" . lsp-treemacs-errors-list)))
;; Microsoft python-language-server support
(use-package lsp-python-ms
  :hook (python-mode . (lambda ()
			 (require 'lsp-python-ms)
			 (lsp-deferred))))
;; Java support
(use-package lsp-java
  :hook (java-mode . (lambda ()
		       (require 'lsp-java)
		       (lsp-deferred))))
;; C/C++/Objective-C support
(use-package ccls
  :defines projectile-project-root-files-top-down-recurring
  :hook ((c-mode c++-mode objc-mode cuda-mode) . (lambda ()
						   (require 'ccls)
						   (lsp-deferred)))
  :config
  (with-eval-after-load 'projectile
    (setq projectile-project-root-files-top-down-recurring
	  (append '("compile_commands.json"
		    ".ccls")
		  projectile-project-root-files-top-down-recurring))))
(provide 'init-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
