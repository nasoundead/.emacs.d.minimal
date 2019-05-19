;; init-neotree.el --- Initialize neotree.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 Vincent Zhang

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
;; neotree: A tree layout file explorer.
;;

;;; Code:
(use-package neotree
  :init
  (setq neo-create-file-auto-open nil
        neo-auto-indent-point nil
        neo-autorefresh nil
        neo-mode-line-type 'none
        neo-window-width 28
        neo-show-updir-line nil
        neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-banner-message nil
        neo-confirm-create-file #'off-p
        neo-confirm-create-directory #'off-p
        neo-show-hidden-files nil
        neo-keymap-style 'concise
        neo-show-hidden-files t
        neo-hidden-regexp-list
        '(;; vcs folders
          "^\\.\\(?:git\\|hg\\|svn\\)$"
          ;; compiled files
          "\\.\\(?:pyc\\|o\\|elc\\|lock\\|css.map\\|class\\)$"
          ;; generated files, caches or local pkgs
          "^\\(?:node_modules\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
          ;; org-mode folders
          "^\\.\\(?:sync\\|export\\|attach\\)$"
          ;; temp files
          "~$"
          "^#.*#$"))

  (with-eval-after-load 'projectile
    (setq projectile-switch-project-action 'neotree-projectile-action))
  :bind
  (([M-f8] . neotree-project-dir-toggle)
   ([f8] . neotree-toggle))
  :config
  (defun neotree-project-dir-toggle ()
    "Open NeoTree using the project root, using find-file-in-project,
or the current buffer directory."
    (interactive)
    (let ((project-dir
           (ignore-errors
           ;;; Pick one: projectile or find-file-in-project
                                        ; (projectile-project-root)
             (ffip-project-root)
             ))
          (file-name (buffer-file-name))
          (neo-smart-open t))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (progn
          (neotree-show)
          (if project-dir
              (neotree-dir project-dir))
          (if file-name
              (neotree-find file-name))))))
;;;###autoload
  (defun +neotree/collapse-or-up ()
    "Collapse an expanded directory node or go to the parent node."
    (interactive)
    (when-let* ((node (neo-buffer--get-filename-current-line)))
      (if (file-directory-p node)
          (if (neo-buffer--expanded-node-p node)
              (+neotree/collapse)
            (neotree-select-up-node))
        (neotree-select-up-node))))

;;;###autoload
  (defun +neotree/collapse ()
    "Collapse a neotree node."
    (interactive)
    (when-let* ((node (neo-buffer--get-filename-current-line)))
      (when (file-directory-p node)
        (neo-buffer--set-expand node nil)
        (neo-buffer--refresh t))
      (when neo-auto-indent-point
        (neo-point-auto-indent))))

;;;###autoload
  (defun +neotree/expand-or-open ()
    "Expand or open a neotree node."
    (interactive)
    (when-let* ((node (neo-buffer--get-filename-current-line)))
      (cond ((file-directory-p node)
             (neo-buffer--set-expand node t)
             (neo-buffer--refresh t)
             (when neo-auto-indent-point
               (forward-line)
               (neo-point-auto-indent)))
            (t
             (call-interactively #'neotree-enter)))))
  )




(provide 'init-neotree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-neotree.el ends here
