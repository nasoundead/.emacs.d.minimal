;;; init-ivy.el -*- lexical-binding: t; -*-
;; -*- lexical-binding: t -*-
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

(use-package ivy
  :diminish
  :config
  (setq
   ivy-wrap t
   projectile-completion-system 'ivy
   smex-completion-method 'ivy
   ivy-fixed-height-minibuffer t
                                        ; don't use ^ as initial input
   ivy-initial-inputs-alist nil
   ivy-format-function #'ivy-format-function-line
   ;; disable magic slash on non-match
   ivy-magic-slash-non-match-action nil
   ;; don't show recent files in switch-buffer
   ivy-use-virtual-buffers nil
   ;; ...but if that ever changes, show their full path
   ivy-virtual-abbreviate 'full
   ;; don't quit minibuffer on delete-error
   ivy-on-del-error-function nil
   ;; enable ability to select prompt (alternative to `ivy-immediate-done')
   ivy-use-selectable-prompt t)

  (ivy-mode)
  (general-define-key
   "<C-return>" #'ivy-switch-buffer
   "C-c C-p"    #'counsel-yank-pop
   "C-c C-m"    #'counsel-mark-ring
   "C-c C-r"    #'ivy-resume)
  )

(use-package swiper
  :commands (swiper swiper-all)
  :bind
  (("C-s" . swiper)
   ("C-S-s". swiper-all)
   ("C-M-s" . isearch-forward)))
(use-package counsel
  :diminish
  :config
  (counsel-mode 1)
  :init
  (general-define-key
   [remap apropos]                  #'counsel-apropos
   [remap bookmark-jump]            #'counsel-bookmark
   [remap describe-face]            #'counsel-describe-face
   [remap describe-function]        #'counsel-describe-function
   [remap describe-variable]        #'counsel-describe-variable
   [remap execute-extended-command] #'counsel-M-x
   [remap find-file]                #'counsel-find-file
   [remap find-library]             #'counsel-find-library
   [remap info-lookup-symbol]       #'counsel-info-lookup-symbol
   [remap imenu]                    #'counsel-imenu
   [remap recentf-open-files]       #'counsel-recentf
   [remap org-capture]              #'counsel-org-capture
   [remap swiper]                   #'counsel-grep-or-swiper)
  :config
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
        ;; Add smart-casing and compressed archive searching (-zS) to default
        ;; command arguments:
        counsel-rg-base-command "rg -zS --no-heading --line-number --color never %s ."
        counsel-ag-base-command "ag -zS --nocolor --nogroup %s"
        counsel-pt-base-command "pt -zS --nocolor --nogroup -e %s")
  (general-define-key
   :keymaps 'counsel-ag-map
   [backtab] '+ivy/wgrep-occur      ; search/replace on results
   [tab]     'ivy-call-and-recenter ; preview
   )
  )


(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

(use-package flx
  :defer t  ; is loaded by ivy
  :init
  (setq ivy-re-builders-alist
        '((counsel-ag . ivy--regex-plus)
          (counsel-grep . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy))
        ivy-initial-inputs-alist nil))

;; Used by `counsel-M-x'
(use-package amx
  :config (setq amx-save-file (concat sea-cache-dir "amx-items")))


(provide 'init-ivy)
