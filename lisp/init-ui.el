;;; init-ui.el -*- lexical-binding: t; -*-
;; Title
(setq frame-title-format
      '("GNU Emacs " emacs-version "@" user-login-name " : "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(setq icon-title-format frame-title-format)


(setq custom-safe-themes t)
(use-package doom-themes)
(use-package color-theme-sanityinc-tomorrow)

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

;(add-hook 'after-init-hook 'reapply-themes)

(defun sanityinc-light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (reapply-themes))

(defun sanityinc-dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (reapply-themes))

;; Restore old window configurations
(use-package winner
  :ensure nil
  :init
  ;; undo/redo changes to Emacs' window layout
  (defvar winner-dont-bind-my-keys t) ; I'll bind keys myself
  (autoload 'winner-mode "winner" nil t)
  (add-hook 'sea-init-ui-hook #'winner-mode)
  (setq winner-boring-buffers '("*Completions*"
                                "*Compile-Log*"
                                "*inferior-lisp*"
                                "*Fuzzy Completions*"
                                "*Apropos*"
                                "*Help*"
                                "*cvs*"
                                "*Buffer List*"
                                "*Ibuffer*"
                                "*esh command on file*")))

(use-package switch-window
:init
(setq switch-window-shortcut-style 'qwerty)
(setq switch-window-threshold 2)
(setq switch-window-minibuffer-shortcut ?z)
(setq switch-window-auto-resize-window
      (lambda ()
        (equal (buffer-name) "*scratch*"))) ;when return t, run auto switch
(setq switch-window-default-window-size '(0.8 . 0.6)) ;80% width and 60% height of frame
(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "C-x 1") 'switch-window-then-maximize)
(global-set-key (kbd "C-x 2") 'switch-window-then-split-below)
(global-set-key (kbd "C-x 3") 'switch-window-then-split-right)
(global-set-key (kbd "C-x 0") 'switch-window-then-delete)

(global-set-key (kbd "C-x 4 d") 'switch-window-then-dired)
(global-set-key (kbd "C-x 4 f") 'switch-window-then-find-file)
(global-set-key (kbd "C-x 4 m") 'switch-window-then-compose-mail)
(global-set-key (kbd "C-x 4 r") 'switch-window-then-find-file-read-only)

(global-set-key (kbd "C-x 4 C-f") 'switch-window-then-find-file)
(global-set-key (kbd "C-x 4 C-o") 'switch-window-then-display-buffer)

(global-set-key (kbd "C-x 4 0") 'switch-window-then-kill-buffer))

(use-package windmove
  :ensure nil
  :init (add-hook 'sea-init-ui-hook #'windmove-default-keybindings))


(defvar sea-init-ui-hook nil
  "ui hook")
(defun sea/init-ui (&optional frame)
  "Set the theme and load the font, in that order."
  (reapply-themes)
  (set-face-attribute 'default nil :font "Inconsolata 12")
  (set-fontset-font "fontset-default" 'chinese-gbk "宋体")
  (setq face-font-rescale-alist '(("宋体" . 1.0)
                ("微软雅黑" . 1.0)
                ))
  (use-package unicode-fonts)
  (unicode-fonts-setup)
  (run-hooks 'sea-init-ui-hook))
(add-hook 'after-init-hook #'sea/init-ui)

;; For Windows
(when IS-WIN
  (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
  (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)) 
(when IS-LINUX
;; For Linux
  (global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
  (global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease))

;; Mode-line	 
(use-package hide-mode-line
  :hook (((completion-list-mode
           completion-in-region-mode
           neotree-mode
           treemacs-mode)
          . hide-mode-line-mode)))

;; icons		  
(use-package all-the-icons
  :commands (all-the-icons-octicon all-the-icons-faicon all-the-icons-fileicon
             all-the-icons-wicon all-the-icons-material all-the-icons-alltheicon)
  :init
  (defun sea*disable-all-the-icons-in-tty (orig-fn &rest args)
    (when (display-graphic-p)
      (apply orig-fn args)))
  :config
  (setq inhibit-compacting-font-caches t)
  (set-frame-font "all-the-icons" t)
  ;; all-the-icons doesn't work in the terminal, so we "disable" it.
  (dolist (fn '(all-the-icons-octicon all-the-icons-material
                all-the-icons-faicon all-the-icons-fileicon
                all-the-icons-wicon all-the-icons-alltheicon))
    (advice-add fn :around #'sea*disable-all-the-icons-in-tty)))
	 
  
(provide 'init-ui)
