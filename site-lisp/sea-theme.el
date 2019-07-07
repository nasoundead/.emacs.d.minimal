;;; sea-theme.el --- Theme based on eclipse theme

;; Copyright (C) 2019

;; Author: WangHaibo

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This theme assumes light background.  To load it, use:
;;
;;     (require 'sea-theme)

;;; Code:

(deftheme sea
  "Color theme from Sea.")

(let ((class '((class color) (min-colors 88) (background light)))
      (sea-bg "#ffffff")
      (sea-fg "#000000")
      (sea-const "#110099")
      (sea-comment "#0399FF")
      (sea-error "#FF0000")
      (sea-func "#CC00FF")
      (sea-type "#00AA88")
      (sea-builtin "#006698")
      (sea-string "#C53100")
      (sea-blue-3 "#758BC6")
      (sea-region "#7F5AB6")
      (sea-shadow "grey50"))
  (apply 'custom-theme-set-faces 'sea
         (mapcar
          (lambda (x) `(,(car x) ((,class ,(cdr x)))))
          `((default :foreground ,sea-fg :background ,sea-bg)
            (cursor :background ,sea-fg)
            (shadow :foreground ,sea-shadow)
            (success :foreground ,sea-error)
            (error :foreground ,sea-error :weight bold)
            (warning :foreground "DarkOrange" :weight bold)
            (compilation-warning :underline t :inherit warning)
            (compilation-error :underline t :inherit error)
            (compilation-info :underline t :foreground ,sea-const)
            (highlight :background "darkseagreen2")
            (fringe :background ,sea-bg)
            (region :background ,sea-region :foreground ,sea-bg)
            (secondary-selection :background "#333366" :foreground "#f6f3e8")
            (whitespace-indentation :background "LightYellow" :foreground "lightgray")
            (term)
            ;; (font-lock-negation-char-face :foreground "#e8e2b7")
            (font-lock-builtin-face :foreground ,sea-builtin :bold t)
            (font-lock-comment-face :foreground ,sea-comment :slant italic)
            (font-lock-comment-delimiter-face :foreground ,sea-comment :slant italic)
            (font-lock-constant-face :foreground ,sea-const)
            (font-lock-doc-face :foreground ,sea-string)
            (font-lock-doc-string-face :foreground ,sea-string)
            (font-lock-function-name-face :foreground ,sea-func :bold t)
            (font-lock-keyword-face :foreground ,sea-builtin :weight bold)
            (font-lock-preprocessor-face :foreground ,sea-builtin :bold t)
            (font-lock-regexp-grouping-backslash :foreground ,sea-builtin)
            (font-lock-regexp-grouping-construct :foreground ,sea-builtin)
            (font-lock-string-face :foreground ,sea-string)
            (font-lock-type-face :foreground ,sea-type :underline t :slant italic)
            (font-lock-variable-name-face :foreground ,sea-fg)
            (font-lock-warning-face :foreground ,sea-error)
            (font-lock-doxygen-face :foreground "SaddleBrown" :background "#f7f7f7")
            (org-code :foreground ,sea-builtin :weight bold)
            (org-verbatim :foreground ,sea-const)
            (org-level-1 :weight bold :foreground "black")
            (org-level-2 :foreground ,sea-builtin)
            (org-level-3 :foreground "#123555")
            (org-level-4 :slant normal :foreground "#E3258D")
            (org-level-5 :slant normal :foreground "#0077CC")
            (org-level-6 :slant italic :foreground "#EA6300")
            (org-level-7 :slant italic :foreground "#2EAE2C")
            (org-level-8 :slant italic :foreground "#FD8008")
            (org-block-begin-line :foreground ,sea-const)
            (org-block-end-line :foreground ,sea-const)
            (org-scheduled-previously :foreground ,sea-comment)
            (ido-subdir :weight bold)
            (mode-line :foreground "black" :background "grey80" :box nil)
            (mode-line-inactive :foreground "grey20" :background "grey90" :box nil)
            (minibuffer-prompt :foreground "medium blue")
            (hl-line :background "#e5e4e2")
            ;; defaults
            (mode-line-buffer-id)
            (show-paren-match :background "turquoise")
            (isearch :background "magenta3" :foreground "lightskyblue1")
            (link :foreground "RoyalBlue3" :underline t)
            ;; other packages
            (helm-locate-finish :foreground ,sea-const)
            (aw-mode-line-face :foreground ,sea-string)
            (swiper-match-face-1 :background "#FEEA89")
            (swiper-match-face-2 :background "#fb7905")
            (swiper-match-face-3 :background "#F9A35A")
            (swiper-match-face-4 :background "#F15C79")
            (swiper-line-face :background "#f3d3d3")
            (hydra-face-red :foreground "#cc0000" :bold t)
            (hydra-face-blue :foreground "RoyalBlue3" :bold t)
            (powerline-active1 :background "grey22" :foreground "white" :inherit mode-line)
            (powerline-active2 :background "grey40" :foreground "white" :inherit mode-line)
            (powerline-inactive1 :background "grey22" :foreground "white" :inherit mode-line-inactive)
            (powerline-inactive2 :background "grey40" :foreground "white" :inherit mode-line-inactive)
            (magit-tag :background "LemonChiffon1" :foreground "goldenrod4")
            (magit-section-heading :inherit header-line)
            (magit-section-highlight :weight bold)
            (magit-diff-context :foreground "grey20")
            (magit-diff-context-highlight :weight bold :foreground "grey20")
            (magit-diff-added :inherit diff-added)
            (magit-diff-added-highlight :inherit diff-added :weight bold)
            (magit-diff-removed :inherit diff-removed)
            (magit-diff-removed-highlight :inherit diff-removed :weight bold)
            (magit-diff-file-heading)
            (magit-diff-file-heading-highlight :weight bold)
            (magit-diff-file-heading-selection :foreground "red")
            (magit-diff-hunk-heading :inherit diff-hunk-header)
            (magit-diff-hunk-heading-highlight :inherit diff-hunk-header :weight bold)
            (magit-hash :foreground "firebrick")
            (magit-branch-remote :background "Grey85" :foreground "OliveDrab4" :box t)
            (magit-branch-local :background "Grey85" :foreground "LightSkyBlue4" :box t)
            (ivy-highlight-face)
            (ivy-posframe :background "#eeeeee" :foreground "#000000")
            (wgrep-face :foreground ,sea-comment)
            (cider-instrumented-face)))))

(custom-theme-set-variables
 'sea
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682"
                            "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"]))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide 'sea-theme)

;;; sea-theme.el ends here
