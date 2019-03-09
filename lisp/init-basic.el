;;; init-basic.elAuthor: Haibo Wang <nasoundead@163.com>;;; Code:;; Key Modifiers(setq-default inhibit-startup-message t gnus-inhibit-startup-message t ;auto-save-mode nil ;make-backup-files nil indent-tabs-mode nil ansi-color-for-comint-mode t bidi-display-reordering nil ; disable bidirectional text for tiny performance boost blink-matching-paren nil    ; don't blink--too distracting compilation-always-kill t        ; kill compilation process before starting another compilation-ask-about-save nil   ; save all buffers on `compile' compilation-scroll-output 'first-error confirm-nonexistent-file-or-buffer t cursor-in-non-selected-windows nil  ; hide cursors in other windows display-line-numbers-width 3 enable-recursive-minibuffers nil frame-inhibit-implied-resize t ;; remove continuation arrow on right fringe fringe-indicator-alist (delq (assq 'continuation fringe-indicator-alist)       fringe-indicator-alist) highlight-nonselected-windows nil image-animate-loop t indicate-buffer-boundaries nil indicate-empty-lines nil inhibit-compacting-font-caches t max-mini-window-height 0.3 mode-line-default-help-echo nil ; disable mode-line mouseovers mouse-yank-at-point t           ; middle-click paste at point, not at click resize-mini-windows 'grow-only  ; Minibuffer resizing show-help-function nil          ; hide :help-echo text split-width-threshold 160       ; favor horizontal splits uniquify-buffer-name-style 'forward use-dialog-box nil              ; always avoid GUI visible-cursor nil x-stretch-cursor nil ;; defer jit font locking slightly to [try to] improve Emacs performance jit-lock-defer-time nil jit-lock-stealth-nice 0.1 jit-lock-stealth-time 0.2 jit-lock-stealth-verbose nil ;; `pos-tip' defaults pos-tip-internal-border-width 6 pos-tip-border-width 1 ;; no beeping or blinking please ring-bell-function #'ignore visible-bell nil ;; don't resize emacs in steps, it looks weird window-resize-pixelwise t frame-resize-pixelwise t kill-ring-max 200 fill-column 80 c-basic-offset   4 tab-width 4 indent-tabs-mode nil save-interprogram-paste-before-kill t)(fset 'yes-or-no-p 'y-or-n-p);; auto close bracket insertion. New in emacs 24(electric-pair-mode 1);; make electric-pair-mode work on more brackets(setq electric-pair-pairs      '(        (?\" . ?\")        (?\{ . ?\})));; store all backup and autosave files in the tmp dir(setq backup-directory-alist      `((".*" . ,temporary-file-directory)))(setq auto-save-file-name-transforms      `((".*" ,temporary-file-directory t)));; revert buffers for changed files(global-auto-revert-mode 1)(setq auto-revert-verbose nil);; highlight matching delimiters(setq show-paren-delay 0.1      show-paren-highlight-openparen t      show-paren-when-point-inside-paren t)(add-hook 'after-init-hook #'show-paren-mode);; Keep track of recently opened files(use-package recentf  :init  (add-hook 'find-file-hook (lambda ()                              (unless recentf-mode                                (recentf-mode)                                (recentf-track-opened-file))))  :config  (setq recentf-save-file (concat sea-cache-dir "recentf")        recentf-max-menu-items 0        recentf-max-saved-items 300        recentf-filename-handlers '(file-truename)        recentf-exclude        (list "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"              "^/var/folders/.+$"              ;; ignore private sea temp files (but not all of them)              (concat "^" (file-truename sea-cache-dir)))))(use-package savehist  :ensure nil  :init  (setq enable-recursive-minibuffers t ; Allow commands in minibuffers        history-length 1000        savehist-file (concat sea-cache-dir "history")        savehist-additional-variables '(mark-ring                                        global-mark-ring                                        search-ring                                        regexp-search-ring                                        extended-command-history)        savehist-autosave-interval 60)  (add-hook 'after-init-hook #'savehist-mode));; History;; Emacsag 25 has a proper mode for `save-place'(setq save-place-file (concat sea-cache-dir "places"))(save-place-mode 1)(when IS-WIN  ;; make PC keyboard's Win key or other to type Super or Hyper, for emacs running on Windows.  (setq w32-pass-lwindow-to-system nil)  (setq w32-lwindow-modifier 'super) ; Left Windows key  (setq w32-pass-rwindow-to-system nil)  (setq w32-rwindow-modifier 'super) ; Right Windows key  (setq w32-pass-apps-to-system nil)  (setq w32-apps-modifier 'hyper) ; Menu/App key  );; coding; (defun windows-shell-mode-coding ()  ; (set-buffer-file-coding-system 'gbk)  ; (set-buffer-process-coding-system 'gbk 'gbk)); (defun python-encode-in-org-babel-execute (func body params)  ; (let ((coding-system-for-write 'utf-8))    ; (funcall func body params))); (cond ; ((when IS-WIN)  ; (set-language-environment "chinese-gbk")  ; (prefer-coding-system 'utf-8)  ; (set-terminal-coding-system 'gbk)  ; (modify-coding-system-alist 'process "*" 'gbk)  ; (add-hook 'shell-mode-hook #'windows-shell-mode-coding)  ; (add-hook 'inferior-python-mode-hook #'windows-shell-mode-coding)  ; (advice-add #'org-babel-execute:python :around              ; #'python-encode-in-org-babel-execute)) ; (t  ; (set-language-environment "UTF-8")  ; (prefer-coding-system 'utf-8)));; Environment(when (or IS-MAC IS-LINUX)  (use-package exec-path-from-shell    :init    (setq exec-path-from-shell-check-startup-files nil)    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"))    (setq exec-path-from-shell-arguments '("-l"))    (exec-path-from-shell-initialize)));; Show native line numbers if possible, otherwise use linum(if (fboundp 'display-line-numbers-mode)    (add-hook 'prog-mode-hook #'display-line-numbers-mode)  (use-package linum-off    :demand    :defines linum-format    :hook (after-init . global-linum-mode)    :config    (setq linum-format "%4d ")    ;; Highlight current line number    (use-package hlinum      :defines linum-highlight-in-all-buffersp      :hook (global-linum-mode . hlinum-activate)      :init      (setq linum-highlight-in-all-buffersp t)      (custom-set-faces       `(linum-highlight-face         ((t (:inherit 'default :background ,(face-background 'default) :foreground ,(face-foreground 'default)))))))))(use-package paradox  :ensure t  :defer t  :config  (setq paradox-spinner-type 'progress-bar        paradox-execute-asynchronously t))(use-package which-key  :diminish which-key-mode  :init  (which-key-mode)  :config  (setq which-key-sort-order #'which-key-prefix-then-key-order        which-key-sort-uppercase-first nil        which-key-add-column-padding 1        which-key-max-display-columns nil        which-key-min-display-lines 5)  ;; embolden local bindings  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)  (which-key-setup-side-window-bottom));; Display Time(use-package time  :ensure nil  :unless (display-graphic-p)  :hook (after-init . display-time-mode)  :init  (setq display-time-24hr-format t)  (setq display-time-day-and-date t));; Kill & Mark things easily(use-package easy-kill  :bind (([remap kill-ring-save] . easy-kill)         ([remap mark-sexp] . easy-mark)));; Interactively insert items from kill-ring(use-package browse-kill-ring  :init (add-hook 'after-init-hook #'browse-kill-ring-default-keybindings));; Treat undo history as a tree(use-package undo-tree  :diminish undo-tree-mode  ;; :ensure quelpa  ;; :quelpa (undo-tree :fetcher git :url "http://www.dr-qubit.org/git/undo-tree.git")  ;; :defer t  :init (add-hook 'after-init-hook #'global-undo-tree-mode)  :config  (setq   undo-tree-auto-save-history nil   undo-tree-history-directory-alist `(("." . ,(concat sea-cache-dir "undo/")))))(use-package crux  :init  (global-set-key (kbd "C-c o") #'crux-open-with)  (global-set-key (kbd "C-c d") #'crux-duplicate-current-line-or-region)  (global-set-key (kbd "C-c M-d") #'crux-duplicate-and-comment-current-line-or-region)  (global-set-key (kbd "C-c D") #'crux-delete-file-and-buffer)  (global-set-key [(control shift return)] #'crux-smart-open-line-above)  (global-set-key [(shift return)] #'crux-smart-open-line)  (global-set-key (kbd "C-c f") #'crux-recentf-find-file)  (global-set-key (kbd "C-c k") #'crux-kill-other-buffers)  (global-set-key (kbd "C-c c") #'crux-cleanup-buffer-or-region)  (global-set-key (kbd "C-c r") #'crux-rename-buffer-and-file)  (global-set-key (kbd "C-c i") #'crux-ispell-word-then-abbrev)  (global-set-key (kbd "C-^") #'crux-top-join-line)  (global-set-key (kbd "C-x C-u") #'crux-upcase-region)  (global-set-key (kbd "C-x C-l") #'crux-downcase-region)  (global-set-key (kbd "C-x M-c") #'crux-downcase-region)  (global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)  (global-set-key [remap kill-whole-line] #'crux-kill-whole-line)  (setq save-abbrevs 'silently)  (setq-default abbrev-mode t)  :config  (crux-with-region-or-buffer indent-region)  (crux-with-region-or-buffer untabify)  (crux-with-region-or-line comment-or-uncomment-region)  (crux-with-region-or-point-to-eol kill-ring-save))(use-package pcre2el  :commands rxt-quote-pcre);; Edit multiple regions in the same way simultaneously(use-package iedit  :defines desktop-minor-mode-table  :bind (("C-;" . iedit-mode)         ("C-x r RET" . iedit-rectangle-mode)         :map isearch-mode-map ("C-;" . iedit-mode-from-isearch)         :map esc-map ("C-;" . iedit-execute-last-modification)         :map help-map ("C-;" . iedit-mode-toggle-on-function))  :config  ;; Avoid restoring `iedit-mode'  (with-eval-after-load 'desktop    (add-to-list 'desktop-minor-mode-table                 '(iedit-mode nil))));; Increase selected region by semantic units(use-package expand-region  :bind ("C-=" . er/expand-region));; Multiple cursors(use-package multiple-cursors  :bind (("C-S-c C-S-c"   . mc/edit-lines)         ("C->"           . mc/mark-next-like-this)         ("C-<"           . mc/mark-previous-like-this)         ("C-c C-<"       . mc/mark-all-like-this)         ("C-M->"         . mc/skip-to-next-like-this)         ("C-M-<"         . mc/skip-to-previous-like-this)         ("s-<mouse-1>"   . mc/add-cursor-on-click)         ("C-S-<mouse-1>" . mc/add-cursor-on-click)         :map mc/keymap         ("C-|" . mc/vertical-align-with-space)));; Smartly select region, rectangle, multi cursors(use-package smart-region  :hook (after-init . smart-region-on));; An all-in-one comment command to rule them all(use-package comment-dwim-2  :bind ("M-;" . comment-dwim-2));; Drag stuff (lines, words, region, etc...) around(use-package drag-stuff  :diminish  :commands drag-stuff-define-keys  :hook (after-init . drag-stuff-global-mode)  :config  (add-to-list 'drag-stuff-except-modes 'org-mode)  (drag-stuff-define-keys));; Hungry deletion(use-package hungry-delete  :diminish  :hook (after-init . global-hungry-delete-mode)  :config (setq-default hungry-delete-chars-to-skip " \t\f\v"));; Framework for mode-specific buffer indexes(use-package imenu  :ensure nil  :bind (("C-." . imenu)));; Drag stuff (lines, words, region, etc...) around(use-package drag-stuff  :diminish  :commands drag-stuff-define-keys  :hook (after-init . drag-stuff-global-mode)  :config  (add-to-list 'drag-stuff-except-modes 'org-mode)  (drag-stuff-define-keys));; Move to the beginning/end of line or code(use-package mwim  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)         ([remap move-end-of-line]       . mwim-end-of-code-or-line)))(use-package session  :init  (setq session-save-file (expand-file-name ".session" sea-cache-dir))  (setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")  (setq session-save-file-coding-system 'utf-8)  (add-hook 'after-init-hook 'session-initialize));; save a list of open files in ~/.emacs.d/.emacs.desktop(setq desktop-path (list sea-cache-dir)      desktop-auto-save-timeout 600)(desktop-save-mode 1)(use-package flycheck  ;; :init (add-hook 'after-init-hook #'global-flycheck-mode)  :config  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))  ;; (setq flycheck-indication-mode 'right-fringe)  ;; (setq flycheck-emacs-lisp-load-path 'inherit)  )(use-package aggressive-indent  :init  (dolist (hook '(emacs-lisp-mode-hook css-mode-hook))    (add-hook hook #'aggressive-indent-mode)))(use-package avy  :bind  ("C-:" . avy-goto-char)  ("C-'". avy-goto-char-2)  )  (use-package dash    :ensure t    :defer t)(use-package f  :ensure t  :defer t)(use-package s  :ensure t  :defer t)(provide 'init-basic);;; base ends here