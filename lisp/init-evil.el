
(use-package evil
  :ensure t
  :init
  (evil-mode 1))

; (use-package evil-leader
  ; :ensure t
  ; :config
  ; (global-evil-leader-mode)
  ; (evil-leader/set-leader "<SPC>")
  ; )

; (evil-leader/set-key
    ; "ff" 'counsel-find-file
    ; "bb" 'ivy-switch-buffer
    ; "bp" 'previous-buffer
    ; "bn" 'next-buffer
    ; "bd" 'kill-this-buffer
    ; "<SPC>" 'counsel-M-x
    ; "wd" 'delete-window
    ; "wm" 'delete-other-windows
    ; "ws" 'split-window-horizontally-instead
    ; "wv" 'split-window-vertically-instead
    ; "gs" 'magit-status
    ; )

(general-create-definer my-leader-def
  ;; :prefix my-leader
  :prefix "SPC")

(general-create-definer my-buffer-leader-def
  ;; :prefix my-buffer-leader
  :prefix "SPC b")

(general-create-definer my-window-leader-def
  ;; :prefix my-window-leader
  :prefix "SPC w")

(general-create-definer my-file-leader-def
  ;; :prefix my-file-leader
  :prefix "SPC f")

(general-create-definer my-local-leader-def
  ;; :prefix my-local-leader
  :prefix "SPC m")

;; ** Global Keybindings
(my-leader-def
  :keymaps 'normal
  "a" 'org-agenda
  "B" 'counsel-bookmark
  "c" 'org-capture
  "<SPC>" 'counsel-M-x
  )
(my-buffer-leader-def
  :keymaps 'normal
  "b" 'ivy-switch-buffer
  "p" 'previous-buffer
  "n" 'next-buffer
  "d" 'kill-this-buffer
  )
(my-file-leader-def
  :keymaps 'normal
  "f" 'counsel-find-file
  )
(my-window-leader-def
  :keymaps 'normal
  "d" 'delete-window
  "m" 'delete-other-windows
  "s" 'split-window-horizontally-instead
  "v" 'split-window-vertically-instead
  "w" 'switch-window)

(define-key evil-normal-state-map (kbd "[b") 'previous-buffer)
(define-key evil-normal-state-map (kbd "]b") 'next-buffer)
(define-key evil-normal-state-map (kbd "M-.") nil)

(general-define-key
 :states 'visual
 "v" #'er/expand-region
 "V" #'er/contract-region)
(general-define-key
 :keymaps 'ivy-minibuffer-map
 "C-j" #'ivy-next-line
 "C-k" #'ivy-previous-line)

(use-package evil-lion
  :ensure t
  :bind (:map evil-normal-state-map
              ("g l " . evil-lion-left)
              ("g L " . evil-lion-right)
              :map evil-visual-state-map
              ("g l " . evil-lion-left)
              ("g L " . evil-lion-right)))

(use-package evil-commentary
  :commands (evil-commentary evil-commentary-yank evil-commentary-line)
  :config (evil-commentary-mode 1))

(use-package evil-easymotion
  :after evil-snipe
  :commands evilem-create)

(use-package evil-escape
  :commands evil-escape-mode
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.25)
  (add-hook 'after-init-hook #'evil-escape-mode)
  :config
  ;; no `evil-escape' in minibuffer
  (push #'minibufferp evil-escape-inhibit-functions))



(use-package evil-mc
  :init
  (defvar evil-mc-key-map (make-sparse-keymap))
  :config
  (global-evil-mc-mode +1)
  ;; disable evil-escape in evil-mc; causes unwanted text on invocation
  (push 'evil-escape-mode evil-mc-incompatible-minor-modes)
  )

(use-package evil-surround
  :defer t
  :init
  (global-evil-surround-mode)
  :ensure t)



(provide 'init-evil)
