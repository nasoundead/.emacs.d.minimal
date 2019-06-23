;; init-modeline.el --- modeline.   -*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;
;; Modeline
;;
;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode))

;; (require 'color-theme)
;; (defun color-theme-dusk ()
;;   "Color theme by Elsa Gonsiorowski, created 2012-05-15.
;; Based on color-theme-midnight by Gordon Messmer and the Xcode dusk theme.
;; Note that there is no background color provided. 282B35.
;; If you want to modify the font as well, you should customize variable
;; `color-theme-legal-frame-parameters' to \"\\(color\\|mode\\|font\\|height\\|width\\)$\".
;; The default setting will prevent color themes from installing specific
;; fonts."
;;   (interactive)
;;   (color-theme-install
;;    '(color-theme-dusk
;;      ((font . "fixed")
;;       (width . 130)
;;       (height . 50)
;;       (background-mode . dark)
;;       (mouse-color . "#FFFFFF")
;;       (cursor-color . "#FFFFFF"))
;;      (default ((t (nill))))
;;      (font-lock-comment-face ((t (:italic t :foreground "#54BE5A"))))
;;      (font-lock-string-face ((t (:foreground "#E1404A"))))
;;      (font-lock-keyword-face ((t (:foreground "#BF369A"))))
;;      (font-lock-warning-face ((t (:bold t :foreground "Pink"))))
;;      (font-lock-constant-face ((t (:foreground "#8B86CE"))))
;;      (font-lock-type-face ((t (:foreground "#f56dca"))))
;;      (font-lock-variable-name-face ((t (:foreground "Yellow"))))
;;      (font-lock-function-name-face ((t (:foreground "#95C76E"))))
;;      (font-lock-builtin-face ((t (:foreground "#D08E5D"))))  ;preprocessor stmts, brown
;;      (hl-line ((t (:background "#112233"))))
;;      (region ((t (:foreground nil :background "#555555"))))
;;      (show-paren-match-face ((t (:bold t :foreground "#ffffff" :background "#050505"))))
;;      (highline-face ((t (:background "#919075")))) ;919075
;;      (setnu-line-number-face ((t (:background "Grey15" :foreground "White" :bold t))))
;;      (show-paren-match-face ((t (:background "grey30"))))
;;      (region ((t (:background "grey15"))))
;;      (highlight ((t (:background "#919075"))))
;;      (secondary-selection ((t (:background "navy"))))
;;      (minibuffer-prompt ((t (:foreground "orange"))))
;;      (link ((t (:foreground "#536fd6" :underline t))))
;;      (widget-field-face ((t (:background "navy" :foreground "white"))))
;;      (widget-single-line-field-face ((t (:background "royalblue" :foreground "white")))))) )

;; (eval-after-load "color-theme"
;;   '(progn
;;      (color-theme-initialize)
;;      (color-theme-dusk)
;;      (when (display-graphic-p)
;;        ;; settings for GUI emacs
;;        (add-to-list 'default-frame-alist '(background-color . "#282B35"))
;;        (add-to-list 'default-frame-alist '(foreground-color . "White"))
;;        )))

;; Mode line settings
;; use setq-default to set it for /all/ modes

(setq-default mode-line-format
              (list
               ;; day and time
               ;; '(:eval (propertize (format-time-string "%c ")
               ;;                     'face 'font-lock-builtin-face))

               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize " %b "
                                   'face
                                   (let ((face (buffer-modified-p)))
                                     (if face 'font-lock-warning-face
                                       'font-lock-type-face))
                                   'help-echo (buffer-file-name)))

               '(:eval (propertize (substring vc-mode 5)
                                   'face 'font-lock-comment-face))


               ;; line and column
               " (" ;; '%02' to set to 2 chars at least; prevents flickering
               (propertize "%02l" 'face 'font-lock-keyword-face) ","
               (propertize "%02c" 'face 'font-lock-keyword-face)
               ") "

               ;; relative position, size of file
               " ["
               (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
               "/"
               (propertize "%I" 'face 'font-lock-constant-face) ;; size
               "] "

               ;; spaces to align right
               '(:eval (propertize
                        " " 'display
                        `((space :align-to (- (+ right right-fringe right-margin)
                                              ,(+ 6 (string-width (let ((sys (coding-system-plist buffer-file-coding-system)))
                                                                    (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                                                                           "UTF-8")
                                                                          (t (upcase (symbol-name (plist-get sys :name))))))))
                                              ,(+ 3 (string-width mode-name)))))))

               ;; "Displays the encoding and eol style of the buffer the same way Atom does."
               '(:eval
                 (propertize
                  (concat (pcase (coding-system-eol-type buffer-file-coding-system)
                            (0 "  LF ")
                            (1 "CRLF ")
                            (2 "  CR "))
                          (let ((sys (coding-system-plist buffer-file-coding-system)))
                            (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                                   "UTF-8")
                                  (t (upcase (symbol-name (plist-get sys :name))))))
                          " ")))

               ;; the current major mode
               ;; (propertize " %m " 'face 'font-lock-string-face)
               '(:eval
                 (propertize
                  (concat (format-mode-line mode-name)
                          (when (stringp mode-line-process)
                            mode-line-process)
                          (and (featurep 'face-remap)
                               (/= text-scale-mode-amount 0)
                               (format "(%+d)" text-scale-mode-amount)))))
               ;;minor-mode-alist
               ))



(provide 'init-modeline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-modeline.el ends here
