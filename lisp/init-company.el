;; init-company.el --- Initialize company configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 Bruce Wong

;; Author: Bruce Wong <nasoundead@163.com>
;; URL: https://github.com/nasoundead/.emacs.d.minimal

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Auto-completion configurations.
;;

;;; Code:

(use-package company
  :diminish company-mode
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-abort
  :init
  (defun sea-company-yasnippet ()
    (interactive)
    (company-abort)
    (call-interactively 'company-yasnippet))
  :bind (("M-/" . company-complete)
         ("<backtab>" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("<tab>" . company-complete-common-or-cycle)
         ("<backtab>" . sea-company-yasnippet)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :hook (after-init . global-company-mode)

  :config
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
        company-tooltip-limit 12            ; bigger popup window
        company-idle-delay .2               ; decrease delay before autocompletion popup shows
        company-echo-delay 0                ; remove annoying blinking
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)

  (use-package company-box
    :if EMACS26+
    :diminish
    :functions (all-the-icons-faicon
                all-the-icons-material
                all-the-icons-octicon
                all-the-icons-alltheicon)
    :hook (company-mode . company-box-mode)
    :config
    ;(setq company-box-backends-colors nil)
    (setq company-box-icons-unknown 'fa_question_circle)
    (with-eval-after-load 'all-the-icons
        (setq company-box-icons-unknown
              (all-the-icons-octicon "file-text" :v-adjust -0.05))

        (setq company-box-icons-elisp
              (list
               (all-the-icons-faicon "cube" :v-adjust -0.0575)        ; Function
               (all-the-icons-faicon "tag" :v-adjust -0.0575)         ; Variable
               (all-the-icons-faicon "cog" :v-adjust -0.0575)         ; Feature
               (all-the-icons-material "palette" :v-adjust -0.2)      ; Face
               ))


        (setq company-box-icons-yasnippet
              (all-the-icons-faicon "code" :v-adjust -0.0575))       ; Snippet

        (setq company-box-icons-lsp
              `(( 1  . ,(all-the-icons-faicon "file-text-o" :v-adjust -0.0575))     ; Text
                ( 2  . ,(all-the-icons-faicon "cube" :v-adjust -0.0575))            ; Method
                ( 3  . ,(all-the-icons-faicon "cube" :v-adjust -0.0575))            ; Function
                ( 4  . ,(all-the-icons-faicon "cube" :v-adjust -0.0575))            ; Constructor
                ( 5  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Field
                ( 6  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Variable
                ( 7  . ,(all-the-icons-faicon "cog" :v-adjust -0.0575))             ; Class
                ( 8  . ,(all-the-icons-faicon "cogs" :v-adjust -0.0575))            ; Interface
                ( 9  . ,(all-the-icons-alltheicon "less"))                          ; Module
                (10  . ,(all-the-icons-faicon "wrench" :v-adjust -0.0575))          ; Property
                (11  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Unit
                (12  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Value
                (13  . ,(all-the-icons-material "content_copy" :v-adjust -0.2))     ; Enum
                (14  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Keyword
                (15  . ,(all-the-icons-material "content_paste" :v-adjust -0.2))    ; Snippet
                (16  . ,(all-the-icons-material "palette" :v-adjust -0.2))          ; Color
                (17  . ,(all-the-icons-faicon "file" :v-adjust -0.0575))            ; File
                (18  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Reference
                (19  . ,(all-the-icons-faicon "folder" :v-adjust -0.0575))          ; Folder
                (20  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; EnumMember
                (21  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Constant
                (22  . ,(all-the-icons-faicon "cog" :v-adjust -0.0575))             ; Struct
                (23  . ,(all-the-icons-faicon "bolt" :v-adjust -0.0575))            ; Event
                (24  . ,(all-the-icons-faicon "tag" :v-adjust -0.0575))             ; Operator
                (25  . ,(all-the-icons-faicon "cog" :v-adjust -0.0575))             ; TypeParameter
                ))))
  ;; Popup documentation for completion candidates
  (when (and (not EMACS26+) (display-graphic-p))
    (use-package company-quickhelp
      :defines company-quickhelp-delay
      :bind (:map company-active-map
                  ("M-h" . company-quickhelp-manual-begin))
      :hook (global-company-mode . company-quickhelp-mode)
      :init (setq company-quickhelp-delay 0.8))))

(provide 'init-company)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-company.el ends here
