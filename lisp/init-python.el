;; init-go.el --- Initialize Golang configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019 WangHaibo

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
;; Golang configurations.
;;

;;; Code:
;;
(use-package python
  :after lsp-mode
  :mode
  ("\\.py\\'" . python-mode)
  ("\\.wsgi$" . python-mode)
  :hook (python-mode . lsp)
  :init
  (setq-default indent-tabs-mode nil)
  :config
  (define-key python-mode-map (kbd "<backtab>") nil)
  (setq python-indent-offset 4))

;; Format using YAPF
;; Install: pip install yapf
(use-package yapfify
  :diminish yapf-mode
  :hook (python-mode . yapf-mode))

;; (use-package anaconda-mode
;;   :ensure t
;;   :after python
;;   :hook
;;   (python-mode . anaconda-mode)
;;   (python-mode . anaconda-eldoc-mode))

;; (use-package company-anaconda
;;   :ensure t
;;   :hook (python-mode . (lambda () (add-to-list (make-local-variable 'company-backends)'(company-anaconda :with company-capf)))))

(use-package pipenv
  :ensure t
  :hook
  ((python-mode . pipenv-mode))
  :init
  (setq pipenv-projectile-after-switch-function
        #'pipenv-projectile-after-switch-extended))

(use-package pyvenv
  :ensure t)

(use-package ein
  :ensure t
  :config
  (setq ein:complete-on-dot -1)
  (cond
   ((eq system-type 'darwin) (setq ein:console-args '("--gui=osx" "--matplotlib=osx" "--colors=Linux")))
   ((eq system-type 'gnu/linux) (setq ein:console-args '("--gui=gtk3" "--matplotlib=gtk3" "--colors=Linux"))))
  (setq ein:query-timeout 1000)
  (defun load-ein ()
    (ein:notebooklist-load)
    (interactive)
    (ein:notebooklist-open)))



(provide 'init-python)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
