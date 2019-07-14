;;; init-basic.elAuthor: Haibo Wang <nasoundead@163.com>;;; Code:;; Key Modifiers(setq-default inhibit-startup-message t gnus-inhibit-startup-message t auto-save-mode nil make-backup-files nil indent-tabs-mode nil confirm-nonexistent-file-or-buffer t display-line-numbers-width 3 enable-recursive-minibuffers nil frame-inhibit-implied-resize t highlight-nonselected-windows nil image-animate-loop t indicate-buffer-boundaries nil indicate-empty-lines nil inhibit-compacting-font-caches t max-mini-window-height 0.3 mouse-yank-at-point t           ; middle-click paste at point, not at click resize-mini-windows 'grow-only  ; Minibuffer resizing split-width-threshold 160       ; favor horizontal splits kill-ring-max 200 fill-column 80 tab-width 4 save-interprogram-paste-before-kill t scroll-step            1 scroll-conservatively  10000 )(fset 'yes-or-no-p 'y-or-n-p)(delete-selection-mode 1)(electric-pair-mode 1)(setq electric-pair-pairs      '(        (?\" . ?\")        (?\{ . ?\})));; revert buffers for changed files(global-auto-revert-mode 1)(setq auto-revert-verbose nil);; highlight matching delimiters(setq show-paren-delay 0.1      show-paren-highlight-openparen t      show-paren-when-point-inside-paren t)(add-hook 'after-init-hook #'show-paren-mode)(when IS-WIN  ;; make PC keyboard's Win key or other to type Super or Hyper, for emacs running on Windows.  (setq w32-pass-lwindow-to-system nil)  (setq w32-lwindow-modifier 'super) ; Left Windows key  (setq w32-pass-rwindow-to-system nil)  (setq w32-rwindow-modifier 'super) ; Right Windows key  (setq w32-pass-apps-to-system nil)  (setq w32-apps-modifier 'hyper) ; Menu/App key  );; (cond;;  ((eq system-type 'windows-nt);;   ;; make PC keyboard's Win key or other to type Super or Hyper;;   ;; (setq w32-pass-lwindow-to-system nil);;   (setq w32-lwindow-modifier 'super)    ; Left Windows key;;   (setq w32-apps-modifier 'hyper)       ; Menu/App ke;;   ;; (w32-register-hot-key [s-]);;   (w32-register-hot-key [s-t]);;   );;  (t nil));; Keep track of recently opened files(use-package recentf  :init  (add-hook 'find-file-hook (lambda ()                              (unless recentf-mode                                (recentf-mode)                                (recentf-track-opened-file))))  :config  (setq recentf-save-file (concat sea-cache-dir "recentf")        recentf-max-menu-items 0        recentf-max-saved-items 300        recentf-filename-handlers '(file-truename)        recentf-exclude        (list "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"              "^/var/folders/.+$"              ;; ignore private sea temp files (but not all of them)              (concat "^" (file-truename sea-cache-dir)))));; Environment(when (or IS-MAC IS-LINUX)  (use-package exec-path-from-shell    :init    (setq exec-path-from-shell-check-startup-files nil)    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"))    (setq exec-path-from-shell-arguments '("-l"))    (exec-path-from-shell-initialize)));; Show native line numbers if possible, otherwise use linum(if (fboundp 'display-line-numbers-mode)    (add-hook 'prog-mode-hook #'display-line-numbers-mode)  (use-package linum-off    :demand    :defines linum-format    :hook (after-init . global-linum-mode)    :config    (setq linum-format "%4d ")    ;; Highlight current line number    (use-package hlinum      :defines linum-highlight-in-all-buffersp      :hook (global-linum-mode . hlinum-activate)      :init      (setq linum-highlight-in-all-buffersp t)      (custom-set-faces       `(linum-highlight-face         ((t (:inherit 'default :background ,(face-background 'default) :foreground ,(face-foreground 'default)))))))))(use-package which-key  :diminish  :hook (after-init . which-key-mode)  :config  (setq which-key-sort-order #'which-key-prefix-then-key-order        which-key-sort-uppercase-first nil        which-key-add-column-padding 1        which-key-max-display-columns nil        which-key-min-display-lines 5)  ;; embolden local bindings  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)  (which-key-setup-side-window-bottom))(use-package session  :init  (setq session-save-file (expand-file-name ".session" sea-cache-dir))  (setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")  (setq session-save-file-coding-system 'utf-8)  (add-hook 'after-init-hook 'session-initialize));; save a list of open files in ~/.emacs.d/.emacs.desktop(setq desktop-path (list sea-cache-dir)      desktop-auto-save-timeout 600)(desktop-save-mode 1);; History;; Emacsag 25 has a proper mode for `save-place'(setq save-place-file (concat sea-cache-dir "places"))(save-place-mode 1)(use-package savehist  :ensure nil  :init  (setq enable-recursive-minibuffers t ; Allow commands in minibuffers        history-length 1000        savehist-file (concat sea-cache-dir "history")        savehist-additional-variables '(mark-ring                                        global-mark-ring                                        search-ring                                        regexp-search-ring                                        extended-command-history)        savehist-autosave-interval 60)  (add-hook 'after-init-hook #'savehist-mode))(use-package flycheck  :init (add-hook 'after-init-hook #'global-flycheck-mode)  :config  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))  (setq flycheck-indication-mode 'right-fringe)  (setq flycheck-emacs-lisp-load-path 'inherit)  compilation-scroll-output 'first-error  );; ediff(setq ediff-diff-options "-w"      ediff-split-window-function #'split-window-horizontally      ediff-window-setup-function #'ediff-setup-windows-plain)(use-package dash  :ensure t  :defer t)(use-package f  :ensure t  :defer t)(use-package s  :ensure t  :defer t)(provide 'init-basic);;; base ends here