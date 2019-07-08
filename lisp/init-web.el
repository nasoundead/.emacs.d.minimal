;;; init-web.el --- Configure of web -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package web-mode
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tmpl\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  ;; Complete for web,html,emmet,jade,slim modes
  (with-eval-after-load 'company
    (use-package company-web
      :init
      (add-hook 'web-mode-hook (lambda ()
                                 (set (make-local-variable 'company-backends)
                                      '(company-tern company-web-html company-yasnippet company-files))))
      ;; Enable JavaScript completion between <script>...</script> etc.
      (advice-add 'company-tern :before
                  #'(lambda (&rest _)
                      (if (equal major-mode 'web-mode)
                          (let ((web-mode-cur-language
                                 (web-mode-language-at-pos)))
                            (if (or (string= web-mode-cur-language "javascript")
                                    (string= web-mode-cur-language "jsx"))
                                (unless tern-mode (tern-mode))
                              (if tern-mode (tern-mode -1))))))))))
(use-package emmet-mode
  :hook
  (web-mode-hook . emmet-mode)
  (sgml-mode-hook . emmet-mode)
  (css-mode-hook . emmet-mode))

(use-package css-mode
  :config
  (setq css-indent-offset 2))

;; JSON mode
(use-package json-mode)

;; Improved JavaScript editing mode
(use-package js2-mode
  :mode "\\.js$"
  :interpreter "node"
  :init
  (add-hook 'js2-mode-hook
            (lambda ()
              (setq js2-basic-offset 2)
              (js2-highlight-unused-variables-mode 1)
              (js2-imenu-extras-mode 1)))
  :config
  (use-package js2-refactor
    :diminish js2-refactor-mode
    :init (add-hook 'js2-mode-hook #'js2-refactor-mode)
    :config (js2r-add-keybindings-with-prefix "C-c C-m")))


;; Typescript Interactive Development Environment
(use-package tide
  :diminish tide-mode
  :init
  (defun setup-tide-mode ()
    "Setup tide mode."
    (interactive)
    (tide-setup)
    (eldoc-mode 1)
    (tide-hl-identifier-mode 1))

  (add-hook 'typescript-mode-hook #'setup-tide-mode)

  (with-eval-after-load 'js2-mode
    (add-hook 'js2-mode-hook #'setup-tide-mode))

  (add-hook 'before-save-hook #'tide-format-before-save)
  :config
  (setq tide-format-options
        '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions
          t
          :placeOpenBraceOnNewLineForFunctions
          nil))

  (with-eval-after-load 'company
    (cl-pushnew 'company-tide company-backends)))


;; Format HTML, CSS and JavaScript/JSON by js-beautify
(use-package web-beautify
  :init
  ;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
  (eval-after-load 'js
    '(add-hook 'js-mode-hook
               (lambda ()
                 (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))
  (eval-after-load 'json-mode
    '(add-hook 'json-mode-hook
               (lambda ()
                 (add-hook 'before-save-hook 'web-beautify-js-buffer t t))))
  (eval-after-load 'sgml-mode
    '(add-hook 'html-mode-hook
               (lambda ()
                 (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))
  (eval-after-load 'web-mode
    '(add-hook 'web-mode-hook
               (lambda ()
                 (add-hook 'before-save-hook 'web-beautify-html-buffer t t))))
  (eval-after-load 'css-mode
    '(add-hook 'css-mode-hook
               (lambda ()
                 (add-hook 'before-save-hook 'web-beautify-css-buffer t t))))
  :config
  ;; Set indent size to 2
  (setq web-beautify-args '("-s" "2" "-f" "-")))


;; Live browser JavaScript, CSS, and HTML interaction
(use-package skewer-mode
  :diminish skewer-mode
  :init
  (add-hook 'js2-mode-hook #'skewer-mode)
  (add-hook 'css-mode-hook #'skewer-css-mode)
  (add-hook 'html-mode-hook #'skewer-html-mode))


(provide 'init-web)
;;; init-web.el ends here
