;; init-window.el --- Initialize window configurations.	-*- lexical-binding: t -*-
;;; Code:

;; Directional window-selection routines
(use-package windmove
  :ensure nil
  :init (add-hook 'after-init-hook #'windmove-default-keybindings))

;; Restore old window configurations
(use-package winner
  :ensure nil
  :init
  (setq winner-boring-buffers '("*Completions*"
                                "*Compile-Log*"
                                "*inferior-lisp*"
                                "*Fuzzy Completions*"
                                "*Apropos*"
                                "*Help*"
                                "*cvs*"
                                "*Buffer List*"
                                "*Ibuffer*"
                                "*esh command on file*"))
  (add-hook 'after-init-hook #'winner-mode))


;; Quickly switch windows
(use-package ace-window
  :bind ("C-x o" . ace-window))

;; Numbered window shortcuts
(use-package window-numbering
  :init (add-hook 'after-init-hook #'window-numbering-mode))
;; (use-package winum
;;   :init
;;   (setq winum-keymap
;;         (let ((map (make-sparse-keymap)))
;;           (define-key map (kbd "C-`") 'winum-select-window-by-number)
;;           (define-key map (kbd "C-²") 'winum-select-window-by-number)
;;           (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
;;           (define-key map (kbd "M-1") 'winum-select-window-1)
;;           (define-key map (kbd "M-2") 'winum-select-window-2)
;;           (define-key map (kbd "M-3") 'winum-select-window-3)
;;           (define-key map (kbd "M-4") 'winum-select-window-4)
;;           (define-key map (kbd "M-5") 'winum-select-window-5)
;;           (define-key map (kbd "M-6") 'winum-select-window-6)
;;           (define-key map (kbd "M-7") 'winum-select-window-7)
;;           (define-key map (kbd "M-8") 'winum-select-window-8)
;;           map))
;;   (setq winum-auto-setup-mode-line nil)
;;   (winum-mode))

;; Zoom window like tmux
(use-package zoom-window
  :bind ("C-x C-z" . zoom-window-zoom)
  :init (setq zoom-window-mode-line-color "DarkGreen"))


;; Easy window config switching
(use-package eyebrowse
  :init (add-hook 'after-init-hook #'eyebrowse-mode))

(provide 'init-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-window.el ends here
