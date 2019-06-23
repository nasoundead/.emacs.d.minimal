;;; init-edit.el --- Configure of edit -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Kill & Mark things easily
(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark)))
;; Interactively insert items from kill-ring
(use-package browse-kill-ring
  :init (add-hook 'after-init-hook #'browse-kill-ring-default-keybindings))
;; Treat undo history as a tree
(use-package undo-tree
  :diminish undo-tree-mode
  :init (add-hook 'after-init-hook #'global-undo-tree-mode)
  :config
  (setq
   undo-tree-auto-save-history nil
   undo-tree-history-directory-alist `(("." . ,(concat sea-cache-dir "undo/")))))
(use-package crux
  :init
  (global-set-key (kbd "C-c o") #'crux-open-with)
  (global-set-key (kbd "C-c d") #'crux-duplicate-current-line-or-region)
  (global-set-key (kbd "C-c M-d") #'crux-duplicate-and-comment-current-line-or-region)
  (global-set-key (kbd "C-c D") #'crux-delete-file-and-buffer)
  (global-set-key [(control shift return)] #'crux-smart-open-line-above)
  (global-set-key [(control return)] #'crux-smart-open-line)
  (global-set-key (kbd "C-c f") #'crux-recentf-find-file)
  (global-set-key (kbd "C-c k") #'crux-kill-other-buffers)
  (global-set-key (kbd "C-c c") #'crux-cleanup-buffer-or-region)
  (global-set-key (kbd "C-c r") #'crux-rename-buffer-and-file)
  (global-set-key (kbd "C-c i") #'crux-ispell-word-then-abbrev)
  (global-set-key (kbd "C-^") #'crux-top-join-line)
  (global-set-key (kbd "C-x C-u") #'crux-upcase-region)
  (global-set-key (kbd "C-x C-l") #'crux-downcase-region)
  (global-set-key (kbd "C-x M-c") #'crux-capitalize-region)
  (global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
  (global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t)
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-line comment-or-uncomment-region)
  (crux-with-region-or-point-to-eol kill-ring-save))
(use-package pcre2el
  :commands rxt-quote-pcre)
;; Edit multiple regions in the same way simultaneously
(use-package iedit
  :defines desktop-minor-mode-table
  :bind (("C-;" . iedit-mode)
         ("C-x r RET" . iedit-rectangle-mode)
         :map isearch-mode-map ("C-;" . iedit-mode-from-isearch)
         :map esc-map ("C-;" . iedit-execute-last-modification)
         :map help-map ("C-;" . iedit-mode-toggle-on-function))
  :config
  ;; Avoid restoring `iedit-mode'
  (with-eval-after-load 'desktop
    (add-to-list 'desktop-minor-mode-table
                 '(iedit-mode nil))))
;; Increase selected region by semantic units
(use-package expand-region
  :bind ("C-=" . er/expand-region))
;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-M-L"   . mc/mark-all-dwim)
         ("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-M->"         . mc/skip-to-next-like-this)
         ("C-M-<"         . mc/skip-to-previous-like-this)
         ("s-<mouse-1>"   . mc/add-cursor-on-click)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         :map mc/keymap
         ("C-|" . mc/vertical-align-with-space)))
;; Smartly select region, rectangle, multi cursors
(use-package smart-region
  :hook (after-init . smart-region-on))
;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))
;; Drag stuff (lines, words, region, etc...) around
(use-package drag-stuff
  :diminish
  :commands drag-stuff-define-keys
  :hook (after-init . drag-stuff-global-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys))
;; Hungry deletion
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"))
;; Framework for mode-specific buffer indexes
;; Drag stuff (lines, words, region, etc...) around
(use-package drag-stuff
  :diminish
  :commands drag-stuff-define-keys
  :hook (after-init . drag-stuff-global-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys))
;; Move to the beginning/end of line or code
(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line]       . mwim-end-of-code-or-line)))



(use-package aggressive-indent
  :init
  (dolist (hook '(emacs-lisp-mode-hook css-mode-hook))
    (add-hook hook #'aggressive-indent-mode)))

(use-package avy
  :bind
  ("C-'" . avy-goto-char))

(use-package hydra)

;; Keybindings
;; Key	Action
;; RET	Visit the result, file or push button at point
;; o	Visit the result in another window
;; n and p	Move between results or buttons
;; N and P	Move between search hits
;; g	Re-run the search
;; TAB	Expand/collapse results for a file
;; C-c C-k	Stop a running search
;; C-u	A prefix argument prevents searches from starting automatically.
;; You can also use M-x imenu to move between files in a results buffer.
;; Minibuffer
;; You use the minibuffer to enter a new search term.
;; You can also reuse a previous search term with M-p in the minibuffer. To edit the default search term, use M-n.
(use-package deadgrep
  :init
  (global-set-key (kbd "<f3>") #'deadgrep))


(use-package visual-regexp
  :init
  (define-key global-map (kbd "C-c q") 'vr/query-replace))

(use-package visual-regexp-steroids)

(provide 'init-edit)
;;; init-edit.el ends here
