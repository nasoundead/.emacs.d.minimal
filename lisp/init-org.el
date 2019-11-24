;; init-go.el --- Initialize Golang configurations.     -*- lexical-binding: t -*-

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

(use-package org
  :ensure t
  :after flyspell
  :config

  ;; Global
  (setq org-hide-emphasis-markers t)
  ;; Editing
  (setq org-list-allow-alphabetical t
        org-highlight-latex-and-related '(latex)
        org-babel-results-keyword "results" ;; Display images directly in the buffer
        org-confirm-babel-evaluate nil
        org-startup-with-inline-images t)
        org-startup-indented t

        org-list-description-max-indent 4
        org-priority-faces
        '((?A . error)
          (?B . warning)
          (?C . success))

        ;; Activate spelling
        (add-hook 'org-mode 'flyspell-mode)
        (add-to-list 'org-export-backends 'md)
        (add-to-list 'ispell-skip-region-alist '("^#+begin_src" . "^#+end_src"))


  ;; (custom-theme-set-faces
  ;;  'user
  ;;  '(variable-pitch ((t (:family "Source Sans Pro" :height 180 :weight light))))
  ;;  '(fixed-pitch ((t ( :family "Inconsolata" :slant normal :weight normal :height 1.0 :width normal)))))

  ;; (custom-theme-set-faces
  ;;  'user
  ;;  '(org-block                 ((t (:inherit fixed-pitch))))
  ;;  '(org-document-info         ((t (:foreground "dark orange"))))
  ;;  '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
  ;;  '(org-link                  ((t (:foreground "royal blue" :underline t))))
  ;;  '(org-meta-line             ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  ;;  '(org-property-value        ((t (:inherit fixed-pitch))) t)
  ;;  '(org-special-keyword       ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  ;;  '(org-tag                   ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
  ;;  '(org-verbatim              ((t (:inherit (shadow fixed-pitch)))))
  ;;  '(org-indent                ((t (:inherit (org-hide fixed-pitch))))))

  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)

  ;; TODO to sort
  (use-package ob-async :ensure t)
  (use-package org-dashboard :ensure t)
  (use-package org-bullets
    :init
    (setq org-bullets-bullet-list '( "⦿" "○"  "✿" "◆"))
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


  (use-package org-notebook :ensure t)


  ;; Todo part
  (setq org-todo-keywords '((sequence
			     "TODO(t)" "IN-PROGRESS(I)"  "|"
			     "DONE(d)" "DEFERRED(f)" "CANCELLED(c@/!)")))

  ;; Priority definition
  (setq org-highest-priority ?A
	org-lowest-priority ?E
	org-default-priority ?C)

  ;; Archiving
  (setq org-archive-mark-done t
	org-log-done 'time
	org-archive-location "%s_archive::* Archived Tasks")

  ;; Refiling
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 3)))
	org-completion-use-ido nil
	org-refile-use-outline-path 'file
	org-outline-path-complete-in-steps nil
	org-refile-allow-creating-parent-nodes 'confirm))



(add-hook 'org-babel-after-execute-hook 'sea/display-inline-images 'append)
(add-hook 'org-mode-hook '(lambda ()(setq truncate-lines t)) 'append)
(defun sea/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))

;; Todo management - some helpers
(defun org-archive-all-done-item ()
  "Archive all item that have with prefix DONE."
  (interactive)
  (save-excursion
    (outline-show-all)
    (goto-char (point-min))
    (if (search-forward-regexp "^[\\*]+ \\(DONE\\|CANCELLED\\)" nil t)
	(progn
	  (goto-char (point-min))
	  (while (search-forward-regexp "^[\\*]+ \\(DONE\\|CANCELLED\\)" nil t)
	    (org-advertized-archive-subtree))
	  (message "Archive finished"))
      (message "No need to archive"))))


(defun org-clean-done-item ()
  "Delete all item that have with prefix DONE."
  (interactive)
  (save-excursion
    (outline-show-all)
    (goto-char (point-min))
    (if (search-forward-regexp "^[\\*]+ \\(DONE\\|CANCELLED\\)" nil t)
	(progn
	  (goto-char (point-min))
	  (while (search-forward-regexp "^[\\*]+ \\(DONE\\|CANCELLED\\)" nil t)
	    (org-cut-subtree))
	  (message "Cleaning DONE tasks finished"))
      (message "No need to clean"))))

;; Calendar / Agenda
(use-package org
  :after (hydra)
  :config

  ;; Todo part
  (setq org-agenda-files '())
  (when (file-exists-p "~/Dropbox/org/todo/todo.org")
    (setq org-agenda-files
	  (append org-agenda-files '("~/Dropbox/org/todo/todo.org"))))

  (when (file-exists-p "~/Dropbox/org/organisation/bookmarks.org")
    (setq org-agenda-files
	  (append org-agenda-files '("~/Dropbox/org/organisation/bookmarks.org"))))

  (when (file-exists-p "~/Calendars")
    (setq org-agenda-files
	  (append org-agenda-files (directory-files "~/Calendars/" t "^.*\\.org$"))))


  ;; Deadline management
  (setq org-agenda-include-diary nil
	org-deadline-warning-days 7
	org-timeline-show-empty-dates t)


  (defun org-agenda-cts ()
    (let ((args (get-text-property
		 (min (1- (point-max)) (point))
		 'org-last-args)))
      (nth 2 args)))
  )
;; Global shortcut to call org agenda
(global-set-key (kbd "<f12>") 'org-agenda)

(use-package org-super-agenda
  :ensure t
  :after (org org-agenda)
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups

	'((:name "Important tasks ":priority "A")
	  (:name "SynSIG" :tag "SynSIG")
	  (:auto-category t)
	  )))

;; TODO: fail gracefully
;; (defun sanityinc/grab-ditaa (url jar-name)
;;   "Download URL and extract JAR-NAME as `org-ditaa-jar-path'."
;;   ;; TODO: handle errors
;;   (message "Grabbing " jar-name " for org.")
;;   (let ((zip-temp (make-temp-name "emacs-ditaa")))
;;     (unwind-protect
;;	(progn
;;	  (when (executable-find "unzip")
;;	    (url-copy-file url zip-temp)
;;	    (shell-command (concat "unzip -p " (shell-quote-argument zip-temp)
;;				   " " (shell-quote-argument jar-name) " > "
;;				   (shell-quote-argument org-ditaa-jar-path)))))
;;       (when (file-exists-p zip-temp)
;;	(delete-file zip-temp)))))
;; (eval-after-load 'ob-ditaa
;;   (unless (and (boundp 'org-ditaa-jar-path)
;;	       (file-exists-p org-ditaa-jar-path))
;;     (let ((jar-name "ditaa0_9.jar")
;;	  (url "http://jaist.dl.sourceforge.net/project/ditaa/ditaa/0.9/ditaa0_9.zip"))
;;       (setq org-ditaa-jar-path (expand-file-name jar-name sea-etc-dir))
;;       (unless (file-exists-p org-ditaa-jar-path)
;;	(sanityinc/grab-ditaa url jar-name)))))

(defvar plantuml-jar-path (expand-file-name "plantuml.jar" sea-etc-dir)
  "plantuml dir")
(defun sea/plantuml-install()
  (let ((url "http://jaist.dl.sourceforge.net/project/plantuml/plantuml.jar"))
    (unless (file-exists-p plantuml-jar-path)
      (url-copy-file url plantuml-jar-path))))
(add-hook 'org-mode-hook '(lambda () (eval-after-load 'ob-plantuml (sea/plantuml-install))))

(defun sea/download-clip()
  "Download Clip.jar to etc dir"
  (let ((jar-name "Clip.jar")
	(url "https://repo.typesafe.com/typesafe/ivy-releases/org.scala-sbt/sbt-launch/0.13.13/sbt-launch.jar"))
    (setq org-clip-jar-path (expand-file-name jar-name sea-etc-dir))
    (unless (file-exists-p org-clip-jar-path)
      (url-copy-file url org-clip-jar-path))))
(add-hook 'org-mode-hook 'sea/download-clip)

(defun org-paste-image ()
  (interactive)
  (let* ((dir (read-directory-name "Dir: ")))
    (insert
     (org-make-link-string
      (concat "file:"
	      (shell-command-to-string
	       (mapconcat #'identity
			  `("java"
			    "-jar"
			    ,(expand-file-name org-clip-jar-path)
			    "--uuid"
			    ,(file-relative-name dir default-directory)
			    )
			  " "
			  )))))))


(use-package plantuml-mode
  :init
  ;; Enable plantuml-mode for PlantUML files
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  )


(use-package org-projectile
  :config
  (org-projectile:per-repo)
  (setq org-projectile:per-repo-filename "todo.org"
	org-agenda-files (append org-agenda-files (org-projectile:todo-files))))


;; Add languages
(use-package ob-ipython :ensure t)
(org-babel-do-load-languages 'org-babel-load-languages
			     '((emacs-lisp . t)
			       (dot . t)
			       (ditaa . t)
			       (R . t)
			       (ipython . t)
			       (ruby . t)
			       (gnuplot . t)
			       (clojure . t)
			       (shell . t)
			       (ledger . t)
			       (org . t)
			       (plantuml . t)
			       (latex . t)))
;; Define specific modes for specific tools
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

;; Block Template
(use-package hydra :ensure t
  :config
  ;; Define the templates
  (setq org-structure-template-alist
	'(("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
	  ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
	  ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
	  ("v" "#+begin_verse\n?\n#+end_verse" "<verse>\n?\n/verse>")
	  ("c" "#+begin_center\n?\n#+end_center" "<center>\n?\n/center>")
	  ("l" "#+begin_export latex\n?\n#+end_export" "<literal style=\"latex\">\n?\n</literal>")
	  ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
	  ("h" "#+begin_export html\n?\n#+end_exrt" "<literal style=\"html\">\n?\n</literal>")
	  ("H" "#+html: " "<literal style=\"html\">?</literal>")
	  ("a" "#+begin_export ascii\n?\n#+end_export")
	  ("A" "#+ascii: ")
	  ("i" "#+index: ?" "#+index: ?")
	  ("I" "#+include: %file ?" "<include file=%file markup=\"?\">")))

  ;; Shortcuts
  (defun hot-expand (str &optional mod)
    "Expand org template."
    (let (text)
      (when (region-active-p)
	(setq text (buffer-substring (region-beginning) (region-end)))
	(delete-region (region-beginning) (region-end)))
      (insert str)
      (org-try-structure-completion)
      (when mod (insert mod) (forward-line))
      (when text (insert text))))

  (defhydra hydra-org-template (:color blue :hint nil)
    "
     Org template

 block               src block         structure
--------------------------------------------------------------------------------------
_c_: center        _s_: src         _L_: LATEX:
_q_: quote         _e_: emacs lisp  _i_: index:
_E_: example       _p_: python      _I_: INCLUDE:
_v_: verse         _P_: perl        _H_: HTML:
_a_: ascii         _u_: Plantuml    _A_: ASCII:
_l_: latex         _d_: ditaa
_h_: html          _S_: shell
"
    ("s" (hot-expand "<s"))
    ("E" (hot-expand "<e"))
    ("q" (hot-expand "<q"))
    ("v" (hot-expand "<v"))
    ("c" (hot-expand "<c"))
    ("l" (hot-expand "<l"))
    ("h" (hot-expand "<h"))
    ("a" (hot-expand "<a"))
    ("L" (hot-expand "<L"))
    ("i" (hot-expand "<i"))
    ("e" (hot-expand "<s" "emacs-lisp"))
    ("p" (hot-expand "<s" "python"))
    ("P" (hot-expand "<s" "perl"))
    ("S" (hot-expand "<s" "sh"))
    ("d" (hot-expand "<s" "ditaa :file CHANGE.png :cache yes"))
    ("u" (hot-expand "<s" "plantuml :file CHANGE.svg :cache yes"))
    ("I" (hot-expand "<I"))
    ("H" (hot-expand "<H"))
    ("A" (hot-expand "<A"))
    ("<" self-insert-command "ins")
    ("ESC" nil "quit"))

  (define-key org-mode-map "<"
    (lambda () (interactive)
      (if (or (region-active-p) (looking-back "^"))
	  (hydra-org-template/body)
	(self-insert-command 1))))
  )
;; id generation
(use-package org-id+
  :ensure quelpa
  :quelpa (org-id+ :repo "seblemaguer/org-id-plus" :fetcher github))


;; Exporting
(use-package htmlize :ensure t)


(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
