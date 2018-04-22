;;; init-org.el
;;; Code:

(use-package org
  :config
  (setq org-directory "~/org-files"
        org-default-notes-file (concat org-directory "/todo.org"))
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda))

(use-package ob-ipython
  :init
  (when sys/windowsp
    (setq ob-ipython-resources-dir "d:/obipy-resources"))
  (with-eval-after-load 'company
    (make-local-variable 'company-backend)
    (cl-pushnew (company-backend-with-yas 'company-ob-ipython) company-backends)))

;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (plantuml . t)
   (ditaa , t)
   (dot . t)
   (emacs-lisp . t)
   (ipython . t)
   (ruby . t)
   (gnuplot . t)
   (sh . t)
   (org . t)
   (latex . t)))
(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
(setq org-plantuml-jar-path
      (expand-file-name "~/forwin/bin/plantuml.jar"))
(setq org-ditaa-jar-path (format "%s%s" ""
                                 (expand-file-name "~/forwin/bin/ditaa.jar")) )

(add-hook 'org-babel-after-execute-hook 'naso/display-inline-images 'append)
(add-hook 'org-mode-hook '(lambda ()(setq truncate-lines t)) 'append)
(defun naso/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

(use-package plantuml-mode
  :init
  (setq plantuml-jar-path
        (expand-file-name
         "~/forwin/bin/plantuml.jar"))
  ;; Enable plantuml-mode for PlantUML files
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  ;; Integration with org-mode
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))


(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


(provide 'init-org)
