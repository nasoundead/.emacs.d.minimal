(use-package evil
  :ensure t
  :init
  (evil-mode 1))

;; (general-create-definer my-leader-def
;;   ;; :prefix my-leader
;;   :prefix "SPC")

;; (general-create-definer my-buffer-leader-def
;;   ;; :prefix my-buffer-leader
;;   :prefix "SPC b")

;; (general-create-definer my-window-leader-def
;;   ;; :prefix my-window-leader
;;   :prefix "SPC w")

;; (general-create-definer my-file-leader-def
;;   ;; :prefix my-file-leader
;;   :prefix "SPC f")

;; (general-create-definer my-local-leader-def
;;   ;; :prefix my-local-leader
;;   :prefix "SPC m")

;; ;; ** Global Keybindings
;; (my-leader-def
;;   :keymaps 'normal
;;   "a" 'org-agenda
;;   "B" 'counsel-bookmark
;;   "c" 'org-capture
;;   "<SPC>" 'counsel-M-x
;;   )
;; (my-buffer-leader-def
;;   :keymaps 'normal
;;   "b" 'ivy-switch-buffer
;;   "p" 'previous-buffer
;;   "n" 'next-buffer
;;   "d" 'kill-this-buffer
;;   )
;; (my-file-leader-def
;;   :keymaps 'normal
;;   "f" 'counsel-find-file
;;   )
;; (my-window-leader-def
;;   :keymaps 'normal
;;   "d" 'delete-window
;;   "m" 'delete-other-windows
;;   "s" 'split-window-horizontally-instead
;;   "v" 'split-window-vertically-instead
;;   "w" 'switch-window)

;; (define-key evil-normal-state-map (kbd "[b") 'previous-buffer)
;; (define-key evil-normal-state-map (kbd "]b") 'next-buffer)
;; (define-key evil-normal-state-map (kbd "M-.") nil)

;; (general-define-key
;;  :states 'visual
;;  "v" #'er/expand-region
;;  "V" #'er/contract-region)

;; (general-define-key
;;  :states '(normal visual)
;;  "gc" #'evil-commentary)

;; (general-define-key
;;  :keymaps 'ivy-minibuffer-map
;;  "C-j" #'ivy-next-line
;;  "C-k" #'ivy-previous-line)

;; (general-define-key
;;  :keymaps 'neotree-mode-map
;;  :states 'normal
;;  "g" #'nil
;;  [tab] #'neotree-quick-look
;;  [return] #'neotree-enter
;;  [backspace] #'evil-window-prev
;;  "c"         #'neotree-create-node
;;  "r"         #'neotree-rename-node
;;  "d"         #'neotree-delete-node
;;  "j"         #'neotree-next-line
;;  "k"         #'neotree-previous-line
;;  "n"         #'neotree-next-line
;;  "p"         #'neotree-previous-line
;;  "h"         #'+neotree/collapse-or-up
;;  "l"         #'+neotree/expand-or-open
;;  "J"         #'neotree-select-next-sibling-node
;;  "K"         #'neotree-select-previous-sibling-node
;;  "H"         #'neotree-select-up-node
;;  "L"         #'neotree-select-down-node
;;  "G"         #'evil-goto-line
;;  "gg"        #'evil-goto-first-line
;;  "v"         #'neotree-enter-vertical-split
;;  "s"         #'neotree-enter-horizontal-split
;;  "q"         #'neotree-hide
;;  "R"         #'neotree-refresh
;;  )
(use-package evil-lion
  :ensure t
  :bind (:map evil-normal-state-map
	  ("g l " . evil-lion-left)
	  ("g L " . evil-lion-right)
	  :map evil-visual-state-map
	  ("g l " . evil-lion-left)
	  ("g L " . evil-lion-right))
  )

(use-package evil-commentary
  :commands (evil-commentary evil-commentary-yank evil-commentary-line)
  :config (evil-commentary-mode 1))

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
  (global-evil-mc-mode 1)
  :config
  ;; disable evil-escape in evil-mc; causes unwanted text on invocation
  (push 'evil-escape-mode evil-mc-incompatible-minor-modes)
  (evil-define-key 'visual evil-mc-key-map
    "A" #'evil-mc-make-cursor-in-visual-selection-end
    "I" #'evil-mc-make-cursor-in-visual-selection-beg)
  )

(use-package evil-surround
  :defer t
  :init
  (global-evil-surround-mode)
  :ensure t)


(use-package evil-vimish-fold
  :defer t
  :init
  (evil-vimish-fold-mode 1)
  :ensure t)


(use-package evil-easymotion
  :defer t
  :ensure t
  :init
  (evilem-default-keybindings "SPC"))

(provide 'init-evil)
