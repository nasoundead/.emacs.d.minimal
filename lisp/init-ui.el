;;; init-ui.el -*- lexical-binding: t; -*-
;; Title
(setq frame-title-format
      '("GNU Emacs " emacs-version "@" user-login-name " : "
	(:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))
(setq icon-title-format frame-title-format)

(defvar sea-init-ui-hook nil
  "ui hook")
(defun sea/init-ui (&optional frame)
  "Set the theme and load the font, in that order."
  (reapply-themes)
  (setq fonts
	(cond ((eq system-type 'darwin)     '("Monaco"    "STHeiti"))
	      ((eq system-type 'gnu/linux)  '("Menlo"     "WenQuanYi Micro Hei Mono"))
	      ((eq system-type 'windows-nt) '("Ubuntu Mono"  "宋体"))))
  (set-face-attribute 'default nil :font
		      (format "%s:pixelsize=%d" (car fonts) 14))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
		      (font-spec :family (car (cdr fonts)))))
  ;; Fix chinese font width and rescale
  (setq face-font-rescale-alist '(("宋体". 1.0) ("Microsoft Yahei" . 1.2) ("WenQuanYi Micro Hei Mono" . 1.2) ("STHeiti". 1.2)))

  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(org-table ((t (:family "Ubuntu Mono"))))
   )

  (require 'font-lock+)
  (run-hooks 'sea-init-ui-hook))
(add-hook 'after-init-hook #'sea/init-ui)

(setq custom-safe-themes t)
(require 'sea-theme)
;; (use-package spacemacs-theme)
;; (use-package color-theme-sanityinc-tomorrow)
;; (use-package doom-themes)
(use-package srcery-theme)
(use-package darktooth-theme)
(use-package zenburn-theme)

;; (setq-default custom-enabled-themes '(darktooth))
;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(defun leuven ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(leuven))
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
				"*Ibuffer*" "*esh command on file*")))

;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode))

(use-package windmove
  :ensure nil
  :init (add-hook 'sea-init-ui-hook #'windmove-default-keybindings))

;; icons
(use-package all-the-icons
  :init
  (require 'all-the-icons)
  (defun sea*disable-all-the-icons-in-tty (orig-fn &rest args)
    (when (display-graphic-p)
      (apply orig-fn args)))
  (setq inhibit-compacting-font-caches t)
  ;; all-the-icons doesn't work in the terminal, so we "disable" it.
  (dolist (fn '(all-the-icons-octicon all-the-icons-material
				      all-the-icons-faicon all-the-icons-fileicon
				      all-the-icons-wicon all-the-icons-alltheicon))
    (advice-add fn :around #'sea*disable-all-the-icons-in-tty)))

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


;; Make "C-x o" prompt for a target window when there are more than 2
(use-package switch-window)
(setq-default switch-window-shortcut-style 'alphabet)
(setq-default switch-window-timeout nil)
(global-set-key (kbd "C-x o") 'switch-window)


(use-package highlight-indent-guides
  :hook ((prog-mode text-mode conf-mode) . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character)
  :config
  (add-hook 'focus-in-hook #'highlight-indent-guides-auto-set-faces)
  ;; `highlight-indent-guides' breaks in these modes
  (add-hook! '(visual-line-mode-hook org-indent-mode-hook)
    (defun +indent-guides-disable-maybe-h ()
      (when highlight-indent-guides-mode
        (highlight-indent-guides-mode -1)))))

;;----------------------------------------------------------------------------
;; When splitting window, show (other-buffer) in the new window
;;----------------------------------------------------------------------------
(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
	(select-window target-window)))))

(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))

(defun sanityinc/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
	   (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") 'sanityinc/toggle-delete-other-windows)

;;----------------------------------------------------------------------------
;; Rearrange split windows
;;----------------------------------------------------------------------------
(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(global-set-key (kbd "C-x |") 'split-window-horizontally-instead)
(global-set-key (kbd "C-x _") 'split-window-vertically-instead)


;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs
(defun sanityinc/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sanityinc/split-window)
      (progn
	(jump-to-register :sanityinc/split-window)
	(setq this-command 'sanityinc/unsplit-window))
    (window-configuration-to-register :sanityinc/split-window)
    (switch-to-buffer-other-window nil)))

(global-set-key (kbd "<f7>") 'sanityinc/split-window)



(defun sanityinc/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
	 (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
	     (if was-dedicated "no longer " "")
	     (buffer-name))))

(global-set-key (kbd "C-c <down>") 'sanityinc/toggle-current-window-dedication)

(provide 'init-ui)
