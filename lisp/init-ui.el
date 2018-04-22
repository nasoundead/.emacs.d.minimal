(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; Title
(setq frame-title-format
      '("GNU Emacs " emacs-version "@" user-login-name " : "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(setq icon-title-format frame-title-format)

;; Menu/Tool/Scroll bars
(unless sys/mac-x-p
  (when (and (fboundp 'menu-bar-mode) menu-bar-mode)
    (menu-bar-mode -1)))
(when (and (fboundp 'tool-bar-mode) tool-bar-mode)
  (tool-bar-mode -1))
(when (and (fboundp 'scroll-bar-mode) scroll-bar-mode)
  (scroll-bar-mode -1))

;; color theme
(setq custom-safe-themes t)
(require-package 'doom-themes)
(require-package 'zenburn-theme)
(require-package 'color-theme-sanityinc-tomorrow)
;; If you don't customize it, this is the theme you get.
;; (require 'modern-light)
;; (require 'modern-solarizeddark)
;; (require 'modern-solarizedlight)
;; (setq-default custom-enabled-themes '(modern-light))
;; (setq-default custom-enabled-themes '(modern-solarizeddark))
;; (setq-default custom-enabled-themes '(modern-solarizedlight))
(setq-default custom-enabled-themes '(sanityinc-tomorrow-night))
;; (setq-default custom-enabled-themes '(sanityinc-tomorrow-bright))
;; (setq-default custom-enabled-themes '(doom-nova))
;; (setq-default custom-enabled-themes '(zenburn))
;; (setq-default custom-enabled-themes '(leuven))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)

;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
(defun zenburn ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(zenburn))
  (reapply-themes))

(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (reapply-themes))


(defun solarized-dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(modern-solarizeddark))
  (reapply-themes))


(defun solarized-light ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(modern-solarizedlight))
  (reapply-themes))

;; set default font in initial window and for any new window
(cond
 ;; case: windows
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  ;; Setting English Font
  ;; (set-default-font "Consolas-11")
  (set-default-font "Ubuntu Mono-12")
  ;; (set-default-font "Source Code Pro-12")
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset (font-spec :family "宋体" :size 14)))
  (setq face-font-rescale-alist '(("宋体" . 1.2) ("Microsoft Yahei" . 1.2) ("WenQuanYi Zen Hei" . 1.2)))

  (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
  (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease) )
 ;; case: Max OS X
 ((string-equal system-type "darwin")  ; Mac OS X
  (when (member "DejaVu Sans Mono" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-10"))
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))))
 ;; case: linux
 ((string-equal system-type "gnu/linux") ; linux
  (when (member "DejaVu Sans Mono" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-10"))
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10")))
  (global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
  (global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)))

;; Line and Column
(setq-default fill-column 80)
(setq column-number-mode t)
(setq line-number-mode t)

;; Show native line numbers if possible, otherwise use linum
(if (version<= "26.0.50" emacs-version )
    (global-display-line-numbers-mode)
  (add-hook 'after-init-hook #'global-linum-mode))

(use-package smooth-scrolling
  :init (add-hook 'after-init-hook #'smooth-scrolling-mode)
  :config (setq smooth-scroll-margin 0))

;; Display Time
(use-package time
  :ensure nil
  :unless (display-graphic-p)
  :preface
  (setq display-time-24hr-format t)
  (setq display-time-day-and-date t)
  :init (add-hook 'after-init-hook #'display-time-mode))

;; Misc
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq visible-bell t)
(size-indication-mode 1)
;; (blink-cursor-mode -1)
(setq track-eol t)                      ; Keep cursor at end of lines. Require line-move-visual is nil.
(setq line-move-visual nil)

;; Don't open a file in a new frame
(when (boundp 'ns-pop-up-frames)
  (setq ns-pop-up-frames nil))

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

;; Toggle fullscreen
(bind-keys ([(control f11)] . toggle-frame-fullscreen)
       ([(control super f)] . toggle-frame-fullscreen) ; Compatible with macOS
       ([(super return)] . toggle-frame-fullscreen)
       ([(meta shift return)] . toggle-frame-fullscreen))

(provide 'init-ui)
