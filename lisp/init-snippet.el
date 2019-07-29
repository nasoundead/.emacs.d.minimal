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

;; (use-package auto-yasnippet
;;   :ensure t
;;   :after yasnippet
;;   :config
;;   (global-set-key (kbd "H-w") #'aya-create)
;;   (global-set-key (kbd "H-y") #'aya-expand)
;;   )

(provide 'init-snippet)
