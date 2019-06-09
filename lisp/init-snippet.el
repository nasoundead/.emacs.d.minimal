;;; init-snippet.el -*- lexical-binding: t; -*-
;; Title
(use-package yasnippet
  :ensure t
  :after company
  :config
  ;; Adding yasnippet support to company
  (add-to-list 'company-backends 'company-yasnippet)
  ;; :init
  ;; Activate global
  (yas-global-mode)
  ;; (yas-reload-all)
  ;; (add-hook 'prog-mode-hook #'yas-minor-mode)
  ;; (add-to-list 'yas-snippet-dir "~/.emacs.d/snippets")
  )

(use-package yasnippet-snippets
  :ensure t
  )

(use-package ivy-yasnippet
  :ensure t)

(use-package yatemplate
  :ensure t
  :after yasnippet
  :config

  ;; Define template directory
  (setq yatemplate-dir (concat config-basedir "~/.emacs.d/templates"))

  ;; Coupling with auto-insert
  (setq auto-insert-alist nil)
  (yatemplate-fill-alist)
  (add-hook 'find-file-hook 'auto-insert)
  )

(provide 'init-snippet)
