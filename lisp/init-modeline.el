;; init-modeline.el --- modeline.	-*- lexical-binding: t -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;
;; Modeline
;;
(defvar sea-memoized-table (make-hash-table :test 'equal :size 10)
  "A lookup table containing memoized functions. The keys are argument lists,
and the value is the function's return value.")
(defun sea-memoize (name)
  "Memoizes an existing function. NAME is a symbol."
  (let ((func (symbol-function name)))
    (put name 'function-documentation
         (concat (documentation func) " (memoized)"))
    (fset name
          `(lambda (&rest args)
             (let ((key (cons ',name args)))
               (or (gethash key sea-memoized-table)
                   (puthash key (apply ',func args)
                            sea-memoized-table)))))))
(defmacro def-memoized! (name arglist &rest body)
  "Create a memoize'd function. NAME, ARGLIST, DOCSTRING and BODY
have the same meaning as in `defun'."
  (declare (indent defun) (doc-string 3))
  `(,(if (bound-and-true-p byte-compile-current-file)
         'with-no-warnings
       'progn)
     (defun ,name ,arglist ,@body)
     (sea-memoize ',name)))
	 
(defmacro def-modeline-segment! (name &rest forms)
  "Defines a modeline segment and byte compiles it."
  (declare (indent defun) (doc-string 2))
  (let ((sym (intern (format "sea-modeline-segment--%s" name))))
    `(progn
       (defun ,sym () ,@forms)
       ,(unless (bound-and-true-p byte-compile-current-file)
          `(let (byte-compile-warnings)
             (byte-compile #',sym))))))

(defsubst sea--prepare-modeline-segments (segments)
  (cl-loop for seg in segments
           if (stringp seg)
            collect seg
           else
            collect (list (intern (format "sea-modeline-segment--%s" (symbol-name seg))))))

(defmacro def-modeline! (name lhs &optional rhs)
  "Defines a modeline format and byte-compiles it. NAME is a symbol to identify
it (used by `sea-modeline' for retrieval). LHS and RHS are lists of symbols of
modeline segments defined with `def-modeline-segment!'.

Example:
  (def-modeline! minimal
    (bar matches \" \" buffer-info)
    (media-info major-mode))
  (sea-set-modeline 'minimal t)"
  (let ((sym (intern (format "sea-modeline-format--%s" name)))
        (lhs-forms (sea--prepare-modeline-segments lhs))
        (rhs-forms (sea--prepare-modeline-segments rhs)))
    `(progn
       (defun ,sym ()
         (let ((lhs (list ,@lhs-forms))
               (rhs (list ,@rhs-forms)))
           (let ((rhs-str (format-mode-line rhs)))
             (list lhs
                   (propertize
                    " " 'display
                    `((space :align-to (- (+ right right-fringe right-margin)
                                          ,(+ 1 (string-width rhs-str))))))
                   rhs-str))))
       ,(unless (bound-and-true-p byte-compile-current-file)
          `(let (byte-compile-warnings)
             (byte-compile #',sym))))))

(defun sea-modeline (key)
  "Returns a mode-line configuration associated with KEY (a symbol). Throws an
error if it doesn't exist."
  (let ((fn (intern (format "sea-modeline-format--%s" key))))
    (when (functionp fn)
      `(:eval (,fn)))))

(defun sea-set-modeline (key &optional default)
  "Set the modeline format. Does nothing if the modeline KEY doesn't exist. If
DEFAULT is non-nil, set the default mode-line for all buffers."
  (when-let* ((modeline (sea-modeline key)))
    (setf (if default
              (default-value 'mode-line-format)
            (buffer-local-value 'mode-line-format (current-buffer)))
          modeline)))
		  
(use-package eldoc-eval
  :config
  (defun +sea-modeline-eldoc (text)
    (concat (when (display-graphic-p)
              (+sea-modeline--make-xpm
               (face-background 'sea-modeline-eldoc-bar nil t)
               +sea-modeline-height
               +sea-modeline-bar-width))
            text))

  ;; Show eldoc in the mode-line with `eval-expression'
  (defun +sea-modeline--show-eldoc (input)
    "Display string STR in the mode-line next to minibuffer."
    (with-current-buffer (eldoc-current-buffer)
      (let* ((str              (and (stringp input) input))
             (mode-line-format (or (and str (or (+sea-modeline-eldoc str) str))
                                   mode-line-format))
             mode-line-in-non-selected-windows)
        (force-mode-line-update)
        (sit-for eldoc-show-in-mode-line-delay))))
  (setq eldoc-in-minibuffer-show-fn #'+sea-modeline--show-eldoc)

  (eldoc-in-minibuffer-mode +1))

;; anzu and evil-anzu expose current/total state that can be displayed in the
;; mode-line.
; (use-package evil-anzu
  ; :requires evil
  ; :init
  ; (add-transient-hook! #'evil-ex-start-search (require 'evil-anzu))
  ; (add-transient-hook! #'evil-ex-start-word-search (require 'evil-anzu))
  ; :config
  ; (setq anzu-cons-mode-line-p nil
        ; anzu-minimum-input-length 1
        ; anzu-search-threshold 250)
  ; ;; Avoid anzu conflicts across buffers
  ; (mapc #'make-variable-buffer-local
        ; '(anzu--total-matched anzu--current-position anzu--state
          ; anzu--cached-count anzu--cached-positions anzu--last-command
          ; anzu--last-isearch-string anzu--overflow-p))
  ; ;; Ensure anzu state is cleared when searches & iedit are done
  ; (add-hook 'isearch-mode-end-hook #'anzu--reset-status t)
  ; (add-hook '+evil-esc-hook #'anzu--reset-status t)
  ; (add-hook 'iedit-mode-end-hook #'anzu--reset-status))


;; Keep `+sea-modeline-current-window' up-to-date
; (defvar +sea-modeline-current-window (frame-selected-window))
; (defun +sea-modeline|set-selected-window (&rest _)
  ; "Sets `+sea-modeline-current-window' appropriately"
  ; (when-let* ((win (frame-selected-window)))
    ; (unless (minibuffer-window-active-p win)
      ; (setq +sea-modeline-current-window win))))

; (add-hook 'window-configuration-change-hook #'+sea-modeline|set-selected-window)
; (add-hook 'focus-in-hook #'+sea-modeline|set-selected-window)
; (advice-add #'handle-switch-frame :after #'+sea-modeline|set-selected-window)
; (advice-add #'select-window :after #'+sea-modeline|set-selected-window)

;; fish-style modeline
(use-package shrink-path
  :commands (shrink-path-prompt shrink-path-file-mixed))


;;
;; Variables
;;

(defvar +sea-modeline-height 25
  "How tall the mode-line should be (only respected in GUI emacs).")

(defvar +sea-modeline-bar-width 3
  "How wide the mode-line bar should be (only respected in GUI emacs).")

(defvar +sea-modeline-vspc
  (propertize " " 'face 'variable-pitch)
  "TODO")

(defvar +sea-modeline-buffer-file-name-style 'truncate-upto-project
  "Determines the style used by `+sea-modeline-buffer-file-name'.

Given ~/Projects/FOSS/emacs/lisp/comint.el
truncate-upto-project => ~/P/F/emacs/lisp/comint.el
truncate-upto-root => ~/P/F/e/lisp/comint.el
truncate-all => ~/P/F/e/l/comint.el
relative-from-project => emacs/lisp/comint.el
relative-to-project => lisp/comint.el
file-name => comint.el")

;; externs
(defvar anzu--state nil)
; (defvar evil-mode nil)
; (defvar evil-state nil)
; (defvar evil-visual-selection nil)
(defvar iedit-mode nil)
(defvar all-the-icons-scale-factor)
(defvar all-the-icons-default-adjust)


;;
;; Custom faces
;;

(defgroup +sea-modeline nil
  ""
  :group 'sea)

(defface sea-modeline-buffer-path
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the dirname part of the buffer path."
  :group '+sea-modeline)

(defface sea-modeline-buffer-file
  '((t (:inherit (mode-line-buffer-id bold))))
  "Face used for the filename part of the mode-line buffer path."
  :group '+sea-modeline)

(defface sea-modeline-buffer-modified
  '((t (:inherit (error bold) :background nil)))
  "Face used for the 'unsaved' symbol in the mode-line."
  :group '+sea-modeline)

(defface sea-modeline-buffer-major-mode
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the major-mode segment in the mode-line."
  :group '+sea-modeline)

(defface sea-modeline-highlight
  '((t (:inherit mode-line-emphasis)))
  "Face for bright segments of the mode-line."
  :group '+sea-modeline)

(defface sea-modeline-panel
  '((t (:inherit mode-line-highlight)))
  "Face for 'X out of Y' segments, such as `+sea-modeline--anzu', and `iedit'"
  :group '+sea-modeline)

(defface sea-modeline-info
  `((t (:inherit (success bold))))
  "Face for info-level messages in the modeline. Used by `*vc'."
  :group '+sea-modeline)

(defface sea-modeline-warning
  `((t (:inherit (warning bold))))
  "Face for warnings in the modeline. Used by `*flycheck'"
  :group '+sea-modeline)

(defface sea-modeline-urgent
  `((t (:inherit (error bold))))
  "Face for errors in the modeline. Used by `*flycheck'"
  :group '+sea-modeline)

;; Bar
(defface sea-modeline-bar '((t (:inherit highlight)))
  "The face used for the left-most bar on the mode-line of an active window."
  :group '+sea-modeline)

(defface sea-modeline-eldoc-bar '((t (:inherit shadow)))
  "The face used for the left-most bar on the mode-line when eldoc-eval is
active."
  :group '+sea-modeline)

(defface sea-modeline-inactive-bar '((t (:inherit warning :inverse-video t)))
  "The face used for the left-most bar on the mode-line of an inactive window."
  :group '+sea-modeline)


;;
;; Modeline helpers
;;

(defsubst active ()
  (eq (selected-window) +sea-modeline-current-window))

;; Inspired from `powerline's `pl/make-xpm'.
(def-memoized! +sea-modeline--make-xpm (color height width)
  "Create an XPM bitmap."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or color "None")))
     (create-image
      (concat
       (format "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
               (length (car data))
               (length data)
               color
               color)
       (apply #'concat
              (cl-loop with idx = 0
                       with len = (length data)
                       for dl in data
                       do (cl-incf idx)
                       collect
                       (concat "\""
                               (cl-loop for d in dl
                                        if (= d 0) collect (string-to-char " ")
                                        else collect (string-to-char "."))
                               (if (eq idx len) "\"};" "\",\n")))))
      'xpm t :ascent 'center))))

(defun +sea-modeline-buffer-file-name ()
  "Propertized `buffer-file-name' based on `+sea-modeline-buffer-file-name-style'."
  (propertize
   (pcase +sea-modeline-buffer-file-name-style
     ('truncate-upto-project (+sea-modeline--buffer-file-name 'shrink))
     ('truncate-upto-root (+sea-modeline--buffer-file-name-truncate))
     ('truncate-all (+sea-modeline--buffer-file-name-truncate t))
     ('relative-to-project (+sea-modeline--buffer-file-name-relative))
     ('relative-from-project (+sea-modeline--buffer-file-name-relative 'include-project))
     ('file-name (propertize (file-name-nondirectory buffer-file-name)
                             'face
                             (let ((face (or (and (buffer-modified-p)
                                                  'sea-modeline-buffer-modified)
                                             (and (active)
                                                  'sea-modeline-buffer-file))))
                               (when face `(:inherit ,face))))))
   'help-echo buffer-file-truename))

(defun +sea-modeline--buffer-file-name-truncate (&optional truncate-tail)
  "Propertized `buffer-file-name' that truncates every dir along path.
If TRUNCATE-TAIL is t also truncate the parent directory of the file."
  (let ((dirs (shrink-path-prompt (file-name-directory buffer-file-truename)))
        (active (active)))
    (if (null dirs)
        (propertize "%b" 'face (if active 'sea-modeline-buffer-file))
      (let ((modified-faces (if (buffer-modified-p) 'sea-modeline-buffer-modified)))
        (let ((dirname (car dirs))
              (basename (cdr dirs))
              (dir-faces (or modified-faces (if active 'sea-modeline-project-root-dir)))
              (file-faces (or modified-faces (if active 'sea-modeline-buffer-file))))
          (concat (propertize (concat dirname
                                      (if truncate-tail (substring basename 0 1) basename)
                                      "/")
                              'face (if dir-faces `(:inherit ,dir-faces)))
                  (propertize (file-name-nondirectory buffer-file-name)
                              'face (if file-faces `(:inherit ,file-faces)))))))))

(defun +sea-modeline--buffer-file-name-relative (&optional include-project)
  "Propertized `buffer-file-name' showing directories relative to project's root only."
  (let ((root (sea-project-root))
        (active (active)))
    (if (null root)
        (propertize "%b" 'face (if active 'sea-modeline-buffer-file))
      (let* ((modified-faces (if (buffer-modified-p) 'sea-modeline-buffer-modified))
             (relative-dirs (file-relative-name (file-name-directory buffer-file-truename)
                                                (if include-project (concat root "../") root)))
             (relative-faces (or modified-faces (if active 'sea-modeline-buffer-path)))
             (file-faces (or modified-faces (if active 'sea-modeline-buffer-file))))
        (if (equal "./" relative-dirs) (setq relative-dirs ""))
        (concat (propertize relative-dirs 'face (if relative-faces `(:inherit ,relative-faces)))
                (propertize (file-name-nondirectory buffer-file-truename)
                            'face (if file-faces `(:inherit ,file-faces))))))))

(defun +sea-modeline--buffer-file-name (truncate-project-root-parent)
  "Propertized `buffer-file-name'.
If TRUNCATE-PROJECT-ROOT-PARENT is t space will be saved by truncating it down
fish-shell style.

Example:
~/Projects/FOSS/emacs/lisp/comint.el => ~/P/F/emacs/lisp/comint.el"
  (let* ((project-root (sea-project-root))
         (file-name-split (shrink-path-file-mixed project-root
                                                  (file-name-directory buffer-file-truename)
                                                  buffer-file-truename))
         (active (active)))
    (if (null file-name-split)
        (propertize "%b" 'face (if active 'sea-modeline-buffer-file))
      (pcase-let ((`(,root-path-parent ,project ,relative-path ,filename) file-name-split))
        (let ((modified-faces (if (buffer-modified-p) 'sea-modeline-buffer-modified)))
          (let ((sp-faces       (or modified-faces (if active 'font-lock-comment-face)))
                (project-faces  (or modified-faces (if active 'font-lock-string-face)))
                (relative-faces (or modified-faces (if active 'sea-modeline-buffer-path)))
                (file-faces     (or modified-faces (if active 'sea-modeline-buffer-file))))
            (let ((sp-props       `(,@(if sp-faces       `(:inherit ,sp-faces))      ,@(if active '(:weight bold))))
                  (project-props  `(,@(if project-faces  `(:inherit ,project-faces)) ,@(if active '(:weight bold))))
                  (relative-props `(,@(if relative-faces `(:inherit ,relative-faces))))
                  (file-props     `(,@(if file-faces     `(:inherit ,file-faces)))))
              (concat (propertize (if truncate-project-root-parent
                                      root-path-parent
                                    (abbreviate-file-name project-root))
                                  'face sp-props)
                      (propertize (concat project "/") 'face project-props)
                      (if relative-path (propertize relative-path 'face relative-props))
                      (propertize filename 'face file-props)))))))))


;;
;; Segments
;;

(def-modeline-segment! buffer-default-directory
  "Displays `default-directory'. This is for special buffers like the scratch
buffer where knowing the current project directory is important."
  (let ((face (if (active) 'sea-modeline-buffer-path)))
    (concat (if (display-graphic-p) " ")
            (all-the-icons-octicon
             "file-directory"
             :face face
             :v-adjust -0.05
             :height 1.25)
            (propertize (concat " " (abbreviate-file-name default-directory))
                        'face face))))

;;
(def-modeline-segment! buffer-info
  "Combined information about the current buffer, including the current working
directory, the file name, and its state (modified, read-only or non-existent)."
  (concat (cond (buffer-read-only
                 (concat (all-the-icons-octicon
                          "lock"
                          :face 'sea-modeline-warning
                          :v-adjust -0.05)
                         " "))
                ((buffer-modified-p)
                 (concat (all-the-icons-faicon
                          "floppy-o"
                          :face 'sea-modeline-buffer-modified
                          :v-adjust -0.0575)
                         " "))
                ((and buffer-file-name
                      (not (file-exists-p buffer-file-name)))
                 (concat (all-the-icons-octicon
                          "circle-slash"
                          :face 'sea-modeline-urgent
                          :v-adjust -0.05)
                         " "))
                ((buffer-narrowed-p)
                 (concat (all-the-icons-octicon
                          "fold"
                          :face 'sea-modeline-warning
                          :v-adjust -0.05)
                         " ")))
          (if buffer-file-name
              (+sea-modeline-buffer-file-name)
            "%b")))

;;
(def-modeline-segment! buffer-info-simple
  "Display only the current buffer's name, but with fontification."
  (propertize
   "%b"
   'face (cond ((and buffer-file-name (buffer-modified-p))
                'sea-modeline-buffer-modified)
               ((active) 'sea-modeline-buffer-file))))

;;
(def-modeline-segment! buffer-encoding
  "Displays the encoding and eol style of the buffer the same way Atom does."
  (concat (pcase (coding-system-eol-type buffer-file-coding-system)
            (0 "LF  ")
            (1 "CRLF  ")
            (2 "CR  "))
          (let ((sys (coding-system-plist buffer-file-coding-system)))
            (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                   "UTF-8")
                  (t (upcase (symbol-name (plist-get sys :name))))))
          "  "))

;;
(def-modeline-segment! major-mode
  "The major mode, including process, environment and text-scale info."
  (propertize
   (concat (format-mode-line mode-name)
           (when (stringp mode-line-process)
             mode-line-process)
           (and (featurep 'face-remap)
                (/= text-scale-mode-amount 0)
                (format " (%+d)" text-scale-mode-amount)))
   'face (if (active) 'sea-modeline-buffer-major-mode)))

;;
(def-modeline-segment! vcs
  "Displays the current branch, colored based on its state."
  (when (and vc-mode buffer-file-name)
    (let* ((backend (vc-backend buffer-file-name))
           (state   (vc-state buffer-file-name backend)))
      (let ((face    'mode-line-inactive)
            (active  (active))
            (all-the-icons-default-adjust -0.1))
        (concat "  "
                (cond ((memq state '(edited added))
                       (if active (setq face 'sea-modeline-info))
                       (all-the-icons-octicon
                        "git-compare"
                        :face face
                        :v-adjust -0.05))
                      ((eq state 'needs-merge)
                       (if active (setq face 'sea-modeline-info))
                       (all-the-icons-octicon "git-merge" :face face))
                      ((eq state 'needs-update)
                       (if active (setq face 'sea-modeline-warning))
                       (all-the-icons-octicon "arrow-down" :face face))
                      ((memq state '(removed conflict unregistered))
                       (if active (setq face 'sea-modeline-urgent))
                       (all-the-icons-octicon "alert" :face face))
                      (t
                       (if active (setq face 'font-lock-doc-face))
                       (all-the-icons-octicon
                        "git-compare"
                        :face face
                        :v-adjust -0.05)))
                " "
                (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                            'face (if active face))
                " ")))))

;;
(defun +sea-ml-icon (icon &optional text face voffset)
  "Displays an octicon ICON with FACE, followed by TEXT. Uses
`all-the-icons-octicon' to fetch the icon."
  (concat (if vc-mode " " "  ")
          (when icon
            (concat
             (all-the-icons-material icon :face face :height 1.1 :v-adjust (or voffset -0.2))
             (if text +sea-modeline-vspc)))
          (when text
            (propertize text 'face face))
          (if vc-mode "  " " ")))

(def-modeline-segment! flycheck
  "Displays color-coded flycheck error status in the current buffer with pretty
icons."
  (when (boundp 'flycheck-last-status-change)
    (pcase flycheck-last-status-change
      ('finished (if flycheck-current-errors
                     (let-alist (flycheck-count-errors flycheck-current-errors)
                       (let ((sum (+ (or .error 0) (or .warning 0))))
                         (+sea-ml-icon "do_not_disturb_alt"
                                        (number-to-string sum)
                                        (if .error 'sea-modeline-urgent 'sea-modeline-warning)
                                        -0.25)))
                   (+sea-ml-icon "check" nil 'sea-modeline-info)))
      ('running     (+sea-ml-icon "access_time" nil 'font-lock-doc-face -0.25))
      ('no-checker  (+sea-ml-icon "sim_card_alert" "-" 'font-lock-doc-face))
      ('errored     (+sea-ml-icon "sim_card_alert" "Error" 'sea-modeline-urgent))
      ('interrupted (+sea-ml-icon "pause" "Interrupted" 'font-lock-doc-face)))))
      ;; ('interrupted (+sea-ml-icon "x" "Interrupted" 'font-lock-doc-face)))))

;;
(defsubst sea-column (pos)
  (save-excursion (goto-char pos)
                  (current-column)))

(def-modeline-segment! selection-info
  "Information about the current selection, such as how many characters and
lines are selected, or the NxM dimensions of a block selection."
  (when (and (active) (or mark-active (eq evil-state 'visual)))
    (let ((reg-beg (region-beginning))
          (reg-end (region-end)))
      (propertize
       (let ((lines (count-lines reg-beg (min (1+ reg-end) (point-max)))))
         (cond ((or (bound-and-true-p rectangle-mark-mode)
                    (eq 'block evil-visual-selection))
                (let ((cols (abs (- (sea-column reg-end)
                                    (sea-column reg-beg)))))
                  (format "%dx%dB" lines cols)))
               ((eq 'line evil-visual-selection)
                (format "%dL" lines))
               ((> lines 1)
                (format "%dC %dL" (- (1+ reg-end) reg-beg) lines))
               (t
                (format "%dC" (- (1+ reg-end) reg-beg)))))
       'face 'sea-modeline-highlight))))


;;
(defun +sea-modeline--macro-recording ()
  "Display current Emacs or evil macro being recorded."
  (when (and (active) (or defining-kbd-macro executing-kbd-macro))
    (let ((sep (propertize " " 'face 'sea-modeline-panel)))
      (concat sep
              (propertize (if (bound-and-true-p evil-this-macro)
                              (char-to-string evil-this-macro)
                            "Macro")
                          'face 'sea-modeline-panel)
              sep
              (all-the-icons-octicon "triangle-right"
                                     :face 'sea-modeline-panel
                                     :v-adjust -0.05)
              sep))))

(defsubst +sea-modeline--anzu ()
  "Show the match index and total number thereof. Requires `anzu', also
`evil-anzu' if using `evil-mode' for compatibility with `evil-search'."
  (when (and anzu--state (not iedit-mode))
    (propertize
     (let ((here anzu--current-position)
           (total anzu--total-matched))
       (cond ((eq anzu--state 'replace-query)
              (format " %d replace " total))
             ((eq anzu--state 'replace)
              (format " %d/%d " here total))
             (anzu--overflow-p
              (format " %s+ " total))
             (t
              (format " %s/%d " here total))))
     'face (if (active) 'sea-modeline-panel))))

(defun sea-themes--overlay-sort (a b)
  (< (overlay-start a) (overlay-start b)))

(defsubst +sea-modeline--iedit ()
  "Show the number of iedit regions matches + what match you're on."
  (when (and iedit-mode iedit-occurrences-overlays)
    (propertize
     (let ((this-oc (or (let ((inhibit-message t))
                          (iedit-find-current-occurrence-overlay))
                        (progn (iedit-prev-occurrence)
                               (iedit-find-current-occurrence-overlay))))
           (length (length iedit-occurrences-overlays)))
       (format " %s/%d "
               (if this-oc
                   (- length
                      (length (memq this-oc (sort (append iedit-occurrences-overlays nil)
                                                  #'sea-themes--overlay-sort)))
                      -1)
                 "-")
               length))
     'face (if (active) 'sea-modeline-panel))))

(def-modeline-segment! matches
  "Displays: 1. the currently recording macro, 2. A current/total for the
current search term (with anzu), 3. The number of substitutions being conducted
with `evil-ex-substitute', and/or 4. The number of active `iedit' regions."
  (let ((meta (concat (+sea-modeline--macro-recording)
                      (+sea-modeline--anzu)
                      (+sea-modeline--iedit))))
     (or (and (not (equal meta "")) meta)
         (if buffer-file-name " %I "))))

;; TODO Include other information
(def-modeline-segment! media-info
  "Metadata regarding the current file, such as dimensions for images."
  (cond ((eq major-mode 'image-mode)
         (cl-destructuring-bind (width . height)
             (image-size (image-get-display-property) :pixels)
           (format "  %dx%d  " width height)))))

(def-modeline-segment! bar
  "The bar regulates the height of the mode-line in GUI Emacs.
Returns \"\" to not break --no-window-system."
  (if (display-graphic-p)
      (+sea-modeline--make-xpm
       (face-background (if (active)
                            'sea-modeline-bar
                          'sea-modeline-inactive-bar)
                        nil t)
       +sea-modeline-height
       +sea-modeline-bar-width)
    ""))


;;
;; Mode lines
;;

(def-modeline! main
  (bar matches " " buffer-info "  %l:%c %p  " selection-info)
  (buffer-encoding major-mode vcs flycheck))

(def-modeline! minimal
  (bar matches " " buffer-info)
  (media-info major-mode))

(def-modeline! special
  (bar matches " " buffer-info-simple "  %l:%c %p  " selection-info)
  (buffer-encoding major-mode flycheck))

(def-modeline! project
  (bar buffer-default-directory)
  (major-mode))

(def-modeline! media
  (bar " %b  ")
  (media-info major-mode))


;;
;; Hooks
;;

(defun +sea-modeline|init ()
  "Set the default modeline."
  (sea-set-modeline 'main t)

  ;; This scratch buffer is already created and doesn't get a modeline. For the
  ;; love of Emacs, someone give the man a modeline!
  (with-current-buffer "*scratch*"
    (sea-set-modeline 'main)))

(defun +sea-modeline|set-special-modeline ()
  (sea-set-modeline 'special))

(defun +sea-modeline|set-media-modeline ()
  (sea-set-modeline 'media))

(defun +sea-modeline|set-project-modeline ()
  (sea-set-modeline 'project))


;;
;; Bootstrap
;;

(add-hook 'sea-init-ui-hook #'+sea-modeline|init)

(add-hook 'image-mode-hook   #'+sea-modeline|set-media-modeline)
(add-hook 'org-src-mode-hook #'+sea-modeline|set-special-modeline)
(add-hook 'circe-mode-hook   #'+sea-modeline|set-special-modeline)
		  
		  
(provide 'init-modeline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-modeline.el ends here
