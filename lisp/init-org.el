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

(use-package org
  :ensure t
  :after flyspell
  :config

  ;; Global
  (setq org-startup-indented t
        org-enforce-todo-dependencies t
        org-cycle-separator-lines 2
        org-blank-before-new-entry '((heading) (plain-list-item . auto))
        org-insert-heading-respect-content nil
        org-reverse-note-order nil
        org-show-following-heading t
        org-show-hierarchy-above t
        org-show-siblings '((default))
        org-id-method 'uuidgen
        org-deadline-warning-days 30
        org-table-export-default-format "orgtbl-to-csv"
        org-src-window-setup 'other-window
        org-clone-delete-id t
        org-cycle-include-plain-lists t
        org-src-fontify-natively t
        org-hide-emphasis-markers t)

  ;; Activate spelling
  (add-hook 'org-mode 'flyspell-mode)
  (add-to-list 'ispell-skip-region-alist '("^#+begin_src" . "^#+end_src"))


  ;; TODO to sort
  (use-package ob-async :ensure t)
  (use-package org-checklist)
  (use-package ob-exp)
  (use-package ox-bibtex)
  (use-package org-protocol)
  (use-package org-dashboard :ensure t)

  ;; Todo part
  (setq org-todo-keywords '((sequence
                             "TODO(t)" "REVIEW(r)" "NEXT(N)" "STARTED(s)"
                             "WAITING(w)" "DELEGATED(e)" "MAYBE(m)" "|"
                             "DONE(d)" "NOTE(n)" "DEFERRED(f)" "CANCELLED(c@/!)"))

        org-todo-state-tags-triggers '(("CANCELLED" ("CANCELLED" . t))
                                       ("WAITING" ("WAITING" . t))
                                       ("HOLD" ("WAITING" . t) ("HOLD" . t))
                                       (done ("WAITING") ("HOLD"))
                                       ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                                       ("IN PROGRESS" ("NEXT") ("WAITING") ("CANCELLED") ("HOLD"))
                                       ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                                       ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

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

;; Todo management - some helpers
(defun org-archive-all-done-item ()
  "Archive all item that have with prefix DONE."
  (interactive)
  (save-excursion
    (show-all)
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
    (show-all)
    (goto-char (point-min))
    (if (search-forward-regexp "^[\\*]+ \\(DONE\\|CANCELLED\\)" nil t)
        (progn
          (goto-char (point-min))
          (while (search-forward-regexp "^[\\*]+ \\(DONE\\|CANCELLED\\)" nil t)
            (org-cut-subtree))
          (message "Cleaning DONE tasks finished"))
      (message "No need to clean"))))

;; Calendar / Agenda
(use-package org-agenda
  :after (hydra org)
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
        org-timeline-show-empty-dates t

        ;;
        org-agenda-category-icon-alist `(
                                         ;; Tools / utils
                                         ("[Ee]macs" ,(format "%s/third_parties/icons/emacs24.png" config-basedir) nil nil :ascent center)
                                         ("[Oo]rg" ,(format "%s/third_parties/icons/org.png" config-basedir) nil nil :ascent center)
                                         ("^[Hh][Tt][Ss]$" ,(format "%s/third_parties/icons/hts.png" config-basedir) nil nil :ascent center)
                                         ("^[Mm]ary[tT]\\{2\\}[sS]$" ,(format "%s/third_parties/icons/marytts.png" config-basedir) nil nil :ascent center)
                                         ("^SFB$" ,(format "%s/third_parties/icons/sfb.png" config-basedir) nil nil :ascent center)
                                         ("[Ss]ystem" ,(format "%s/third_parties/icons/debian.png" config-basedir) nil nil :ascent center)
                                         ("[Tt]ools?" ,(format "%s/third_parties/icons/wrench.png" config-basedir) nil nil :ascent center)
                                         ("[Ex]pe\\(riment\\)s?" ,(format "%s/third_parties/icons/expes.png" config-basedir) nil nil :ascent center)

                                         ;; Admin / meeting
                                         ("[Aa]dmin" ,(format "%s/third_parties/icons/admin.png" config-basedir) nil nil :ascent center)
                                         ("[Mm]eeting" ,(format "%s/third_parties/icons/meeting.png" config-basedir) nil nil :ascent center)
                                         ("[Aa]ppointments?" ,(format "%s/third_parties/icons/appointment.png" config-basedir) nil nil :ascent center)
                                         ("[Vv]isitors" ,(format "%s/third_parties/icons/visitors.png" config-basedir) nil nil :ascent center)
                                         ("synsig" ,(format "%s/third_parties/icons/isca.png" config-basedir) nil nil :ascent center)
                                         ("\\([Tt]rip\\|[Dd]eplacement\\)" ,(format "%s/third_parties/icons/trip.png" config-basedir) nil nil :ascent center)
                                         ("Train" ,(format "%s/third_parties/icons/train.png" config-basedir) nil nil :ascent center)

                                         ;; Deadlines / dates
                                         ("\\([Pp]resentations?\\)" ,(format "%s/third_parties/icons/meeting.png" config-basedir) nil nil :ascent center)
                                         ("\\([Pp]apers?\\|[Bb]lio?\\|[Aa]rticles?\\)" ,(format "%s/third_parties/icons/book.png" config-basedir) nil nil :ascent center)
                                         ("[Mm]ails?" ,(format "%s/third_parties/icons/gnus.png" config-basedir) nil nil :ascent center)
                                         ("[Rr]eview?" ,(format "%s/third_parties/icons/review.png" config-basedir) nil nil :ascent center)

                                         ;; Personnal dates
                                         ("Medical" ,(format "%s/third_parties/icons/medical.png" config-basedir) nil nil :ascent center)
                                         ("\\(Party\\|Celeb\\)" ,(format "%s/third_parties/icons/party.png" config-basedir) nil nil :ascent center)
                                         ("Anniv" ,(format "%s/third_parties/icons/anniversary.png" config-basedir) nil nil :ascent center)
                                         ("\\([Hh]olidays\\|[Vv]acations?\\)" ,(format "%s/third_parties/icons/holidays.png" config-basedir) nil nil :ascent center)

                                         ;; Personnal diverse
                                         ("Music" ,(format "%s/third_parties/icons/music.png" config-basedir) nil nil :ascent center)
                                         ("Book" ,(format "%s/third_parties/icons/book.png" config-basedir) nil nil :ascent center)
                                         ("[Pp]rojects?" ,(format "%s/third_parties/icons/project.png" config-basedir) nil nil :ascent center)
                                         (".*" '(space . (:width (16)))))

        ;; Some commands
        org-agenda-custom-commands '(
                                     ("D" todo "DONE")

                                     ("w" "Work and administrative"
                                      ((agenda)
                                       (tags-todo "WORK")
                                       (tags-todo "OFFICE")
                                       (tags-todo "ADMIN")))

                                     ("p" "personnal"
                                      ((agenda)
                                       (tags-todo "PERSONNAL")))

                                     ("d" "Daily Action List"
                                      ((agenda "" ((org-agenda-ndays 1)
                                                   (org-agenda-sorting-strategy
                                                    '((agenda time-up priority-down tag-up) ))
                                                   (org-deadline-warning-days 0)))))))

  ;; Agenda view shortcuts
  (define-key org-agenda-mode-map "v" 'hydra-org-agenda-view/body)

  (defun org-agenda-cts ()
    (let ((args (get-text-property
                 (min (1- (point-max)) (point))
                 'org-last-args)))
      (nth 2 args)))


  (defhydra hydra-org-agenda-view (:color blue :hint none)
    "
    _d_: ?d? day        _g_: time grid=?g? _a_: arch-trees
    _w_: ?w? week       _[_: inactive      _A_: arch-files
    _t_: ?t? fortnight  _f_: follow=?f?    _r_: report=?r?
    _m_: ?m? month      _e_: entry =?e?    _D_: diary=?D?
    _y_: ?y? year       _q_: quit          _L__l__c_: ?l?
       "

    ("SPC" org-agenda-reset-view)
    ("d" org-agenda-day-view
     (if (eq 'day (org-agenda-cts))
         "[x]" "[ ]"))
    ("w" org-agenda-week-view
     (if (eq 'week (org-agenda-cts))
         "[x]" "[ ]"))
    ("t" org-agenda-fortnight-view
     (if (eq 'fortnight (org-agenda-cts))
         "[x]" "[ ]"))
    ("m" org-agenda-month-view
     (if (eq 'month (org-agenda-cts)) "[x]" "[ ]"))
    ("y" org-agenda-year-view
     (if (eq 'year (org-agenda-cts)) "[x]" "[ ]"))
    ("l" org-agenda-log-mode
     (format "% -3S" org-agenda-show-log))
    ("L" (org-agenda-log-mode '(4)))
    ("c" (org-agenda-log-mode 'clockcheck))
    ("f" org-agenda-follow-mode
     (format "% -3S" org-agenda-follow-mode))
    ("a" org-agenda-archives-mode)
    ("A" (org-agenda-archives-mode 'files))
    ("r" org-agenda-clockreport-mode
     (format "% -3S" org-agenda-clockreport-mode))
    ("e" org-agenda-entry-text-mode
     (format "% -3S" org-agenda-entry-text-mode))
    ("g" org-agenda-toggle-time-grid
     (format "% -3S" org-agenda-use-time-grid))
    ("D" org-agenda-toggle-diary
     (format "% -3S" org-agenda-include-diary))
    ("!" org-agenda-toggle-deadlines)
    ("["
     (let ((org-agenda-include-inactive-timestamps t))
       (org-agenda-check-type t 'timeline 'agenda)
       (org-agenda-redo)))
    ("q" (message "Abort") :exit t)))

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

(use-package org-capture
  :config

  ;; Capture
  (setq org-capture-templates
        `(("b" "Adding book" entry
           (file+headline "~/Dropbox/org/todo/todo.org" "To read")
           (file ,(format "%s/third_parties/org-capture-templates/book.org" config-basedir)))

          ("L" "Bookmark" entry
           (file+olp "~/Dropbox/org/todo/todo.org" "To review" "Bookmarks")
           (file ,(format "%s/third_parties/org-capture-templates/bookmark.org" config-basedir)))

          ("m" "mail" entry
           (file+headline "~/Dropbox/org/todo/todo.org" "Mailing")
           (file ,(format "%s/third_parties/org-capture-templates/mail.org" config-basedir)))

          ("M" "MSP calendar" entry
           (file "~/Calendars/Calendar-MSP.org")
           (file ,(format "%s/third_parties/org-capture-templates/calendar.org" config-basedir)))

          ("P" "Personnal calendar" entry
           (file "~/Calendars/Calendar-Personal.org")
           (file ,(format "%s/third_parties/org-capture-templates/calendar.org" config-basedir)))

          ("r" "RSS" entry
           (file+olp "~/Dropbox/org/todo/todo.org" "To review" "RSS")
           (file ,(format "%s/third_parties/org-capture-templates/rss.org" config-basedir)))

          ("t" "ToDo Entry" entry
           (file+headline "~/Dropbox/org/todo/todo.org" "To sort")
           (file ,(format "%s/third_parties/org-capture-templates/default.org" config-basedir))
           :empty-lines-before 1)))

  (use-package org-chef
    :ensure t
    :config
    (add-to-list 'org-capture-templates
                 '("c" "Cookbook" entry (file "~/Dropbox/recipes/cookbook.org")
                   "%(org-chef-get-recipe-from-url)"
                   :empty-lines 1)))

  ;; Editing
  (setq org-list-allow-alphabetical t
        org-highlight-latex-and-related '(latex)
        org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar"
        org-babel-results-keyword "results" ;; Display images directly in the buffer
        org-confirm-babel-evaluate nil
        org-startup-with-inline-images t)

  (use-package org-notebook :ensure t)

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

                                        ; Define specific modes for specific tools
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
        `      (org-try-structure-completion)
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

  (use-package ox-html
    :after ox
    :requires (htmlize)
    :config
    (setq org-html-xml-declaration '(("html" . "")
                                     ("was-html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
                                     ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>"))
          org-export-html-inline-images t
          org-export-with-sub-superscripts nil
          org-export-html-style-extra "<link rel=\"stylesheet\" href=\"org.css\" type=\"text/css\" />"
          org-export-html-style-include-default nil
          org-export-htmlize-output-type 'css ; Do not generate internal css formatting for HTML exports
          )

    (defun endless/export-audio-link (path desc format)
      "Export org audio links to hmtl."
      (cl-case format
        (html (format "<audio src=\"%s\" controls>%s</audio>" path (or desc "")))))
    (org-add-link-type "audio" #'ignore #'endless/export-audio-link)


    (defun endless/export-video-link (path desc format)
      "Export org video links to hmtl."
      (cl-case format
        (html (format "<video controls src=\"%s\"></video>" path (or desc "")))))
    (org-add-link-type "video" #'ignore #'endless/export-video-link)

    (add-to-list 'org-file-apps '("\\.x?html?\\'" . "/usr/bin/vivaldi-stable %s")))

  (use-package ox-reveal
    :ensure t
    :requires (ox-html htmlize))

  ;; (use-package ox-latex
  :after ox
  :defer t
  :config
  (setq org-latex-listings t
        org-export-with-LaTeX-fragments t
        org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")))

;; LaTex
(use-package ox-latex
  :after ox
  :defer t
  :config
  (setq org-latex-listings t
        org-export-with-LaTeX-fragments t
        org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")))

;; Beamer
(use-package ox-beamer
  :after ox
  :config
  (defun my-beamer-bold (contents backend info)
    (when (eq backend 'beamer)
      (replace-regexp-in-string "\\`\\\\[A-Za-z0-9]+" "\\\\textbf" contents)))
  (add-to-list 'org-export-filter-bold-functions 'my-beamer-bold))

;; Docbook
(setq org-export-docbook-xsl-fo-proc-command "fop %s %s"
      org-export-docbook-xslt-proc-command "xsltproc --output %s /usr/share/xml/docbook/stylesheet/nwalsh/fo/docbook.xsl %s")

;; Markdown
(use-package ox-gfm
  :ensure t
  :after ox
  :config (require 'ox-gfm))

;; Pandoc
(use-package ox-pandoc
  :ensure t
  :after ox
  :defer t
  :config
  ;; default options for all output formats
  (setq org-pandoc-options '((standalone . t))
        ;; cancel above settings only for 'docx' format
        org-pandoc-options-for-docx '((standalone . nil))
        ;; special settings for beamer-pdf and latex-pdf exporters
        org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex"))
        org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex"))))

(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
