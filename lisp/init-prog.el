;; init-prog.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(use-package prog-mode
  :ensure nil
  :init
  ;; Prettify Symbols
  ;; e.g. display “lambda” as “λ”
  (when (boundp 'global-prettify-symbols-mode)
    (add-hook 'after-init-hook #'global-prettify-symbols-mode)
    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (push '("<=" . ?≤) prettify-symbols-alist)))))

;; Jump to definition via `ag'/`rg'/`grep'
(use-package dumb-jump
  :init (add-hook 'after-init-hook #'dumb-jump-mode)
  :config
  (setq dumb-jump-prefer-searcher 'rg)
  (with-eval-after-load 'ivy
    (setq dumb-jump-selector 'ivy)))

(use-package nxml-mode
  :ensure nil
  :mode (("\\.xaml$" . xml-mode)))

(use-package quickrun)
(use-package powershell)
(use-package csharp-mode)
(use-package dockerfile-mode :mode "Dockerfile\\'")
(use-package vimrc-mode)

;; New `conf-toml-mode' in Emacs26
(unless (fboundp 'conf-toml-mode)
  (use-package toml-mode))

(use-package editorconfig
  :diminish editorconfig-mode
  :init (add-hook 'prog-mode-hook #'editorconfig-mode))

;; New `bat-mode' in 25, only use `batch-mode' in 24.
(unless (fboundp 'bat-mode)
  (use-package batch-mode
    :mode (("\\.\\(cmd\\|bat\\)$" . batch-mode))))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode))
  :init
  (when (executable-find "multimarkdown")
    (setq markdown-command "multimarkdown"))
  :config
  ;; On the fly markdown preview
  (use-package flymd
    :bind (:map markdown-mode-map ("C-c C-c f" . flymd-flyit))))

(use-package fish-mode
  :init
  (add-hook 'fish-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        #'fish_indent-before-save))))

(use-package swift-mode
  :config
  (with-eval-after-load 'flycheck
    (use-package flycheck-swift
      :init (flycheck-swift-setup))))

(use-package robot-mode
  :ensure nil
  :load-path "site-lisp"
  :commands robot-mode
  :mode "\\.robot\\'")

(when (maybe-require-package 'origami)
  (add-hook 'prog-mode-hook 'origami-mode)
  (after-load 'origami
    (define-key origami-mode-map (kbd "C-{") 'origami-recursively-toggle-node)
    (define-key origami-mode-map (kbd "C-M-{") 'origami-toggle-all-nodes)))

(use-package lsp-mode
  :init
  (with-eval-after-load 'company
    (use-package company-lsp
      :init (cl-pushnew (company-backend-with-yas 'company-lsp) company-backends))))
(use-package lsp-ui
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (lsp-ui-peek-jump-backward)
  (lsp-ui-peek-jump-forward)
  (with-eval-after-load 'flycheck
    ;; (require 'lsp-flycheck)
    (add-hook 'python-mode-hook 'flycheck-mode))
  )
;; Go support for lsp-mode using Sourcegraph's Go Language Server
;; Install: go get github.com/sourcegraph/go-langserver
(use-package lsp-go
  :commands lsp-go-enable
  :init (add-hook 'go-mode-hook #'lsp-go-enable))

;; Python support for lsp-mode using pyls.
;; Install: pip install python-language-server
(use-package lsp-python
  :commands lsp-python-enable
  :init (add-hook 'python-mode-hook #'lsp-python-enable))

;; Java support for lsp-mode using the Eclipse JDT Language Server.
;; Install:
;; wget http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz
;; tar jdt-language-server-latest.tar.gz -C ~/.emacs.d/eclipse.jdt.ls/server/
(use-package lsp-java
  :commands lsp-java-enable
  :init (add-hook 'java-mode-hook #'lsp-java-enable))

(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
