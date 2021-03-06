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
;; Projectile configurations.
;;

;;; Code:
(use-package projectile
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :init (add-hook 'after-init-hook #'projectile-mode)
  :config
  ;; (setq projectile-mode-line
  ;;       '(:eval (format "[%s]" (projectile-project-name))))
  (setq-default projectile-mode-line-prefix " Proj")

  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" sea-cache-dir))

  (setq projectile-completion-system 'ivy)

  (setq projectile-sort-order 'recentf)
  (setq projectile-use-git-grep t)

  (setq projectile-switch-project-action
        '(lambda ()
           (ignore-errors
             (venv-projectile-auto-workon))
           (projectile-find-file)))

  ;; Faster indexing on Windows
  ;; `ripgrep' is the fastest
  (when IS-WIN
    (when (executable-find "rg")
      (setq projectile-generic-command "rg -0 --files --color=never --hidden")
      (setq projectile-indexing-method 'alien)
      (setq projectile-enable-caching nil))

    ;; FIXME: too slow while getting submodule files on Windows
    (setq projectile-git-submodule-command ""))

  ;; Support Perforce project
  (let ((val (or (getenv "P4CONFIG") ".p4config")))
    (add-to-list 'projectile-project-root-files-bottom-up val)))

;; Group ibuffer's list by project root
(use-package ibuffer-projectile
  :bind ("C-x C-b" . ibuffer)
  :init
  (setq ibuffer-filter-group-name-face 'font-lock-function-name-face)
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-auto-mode 1)
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))


(provide 'init-projectile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-company.el ends here
