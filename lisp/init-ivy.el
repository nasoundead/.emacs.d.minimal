;;; init-ivy.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-
;;; Code:
(use-package counsel
  :diminish ivy-mode counsel-mode
  :bind (("C-s" . counsel-grep-or-swiper)
         ("C-S-s" . swiper-all)
         ("C-c M-x" . execute-extended-command)

         ("C-c C-r" . ivy-resume)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)

         ("C-." . counsel-imenu)
         ("C-x C-r" . counsel-recentf)
         ("C-h F" . counsel-find-library)
         ("C-h u" . counsel-unicode-char)
         ("C-c c" . counsel-colors-emacs)
         ("C-c w" . counsel-colors-web)
         ("C-c i" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c r" . counsel-rg)
         ("C-c P" . counsel-pt)
         ("C-c S" . counsel-ag)
         ("C-c f" . counsel-fzf)
         ("C-c l" . counsel-locate)
         ("C-c L" . counsel-load-library)
         ("C-c C-p" . counsel-package)
         ("C-x j" . counsel-mark-ring)
         ("C-x r b" . counsel-bookmark)
         ("C-x r m" . counsel-bookmark)

         :map ivy-minibuffer-map
         ("C-w" . ivy-yank-word)

         :map counsel-find-file-map
         ("C-h" . counsel-up-directory)

         :map swiper-map
         ("M-%" . swiper-query-replace))
  :init (add-hook 'after-init-hook
                  (lambda ()
                    (ivy-mode 1)
                    (counsel-mode 1)))
  :config
  (setq ivy-use-virtual-buffers t)    ; Enable bookmarks and recentf
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-on-del-error-function nil)
  ;; (setq ivy-initial-inputs-alist nil)

  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))

  (setq swiper-action-recenter t)
  (setq counsel-find-file-at-point t)
  (setq counsel-yank-pop-separator "\n-------\n")

  ;; Use faster search tools: ripgrep or the silver search
  (let ((command
         (cond
          ((executable-find "rg")
           "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
          ((executable-find "ag")
           "ag -i --noheading --nocolor --nofilename --numbers '%s' %s"))))
    (setq counsel-grep-base-command command))

  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read))

  ;; Enhance M-x
  (use-package smex
    :config
    (setq-default smex-save-file (expand-file-name ".smex-items" cache-dir)))

  ;; Additional key bindings for Ivy
  (use-package ivy-hydra
    :bind (:map ivy-minibuffer-map
                ("M-o" . ivy-dispatching-done-hydra)))

)
(provide 'init-ivy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ivy.el ends here
