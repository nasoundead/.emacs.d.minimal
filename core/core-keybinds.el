;;; core-keybinds.el -*- lexical-binding: t; -*-

;; A centralized keybinds system, integrated with `which-key' to preview
;; available keybindings. All built into one powerful macro: `map!'. If evil is
;; never loaded, then evil bindings set with `map!' are ignored (i.e. omitted
;; entirely for performance reasons).

(defvar sea-leader-key "SPC"
  "The leader prefix key for Evil users.

This needs to be changed from $SEADIR/init.el.")

(defvar sea-leader-alt-key "M-SPC"
  "An alternative leader prefix key, used for Insert and Emacs states, and for
non-evil users.

This needs to be changed from $SEADIR/init.el.")

(defvar sea-localleader-key "SPC m"
  "The localleader prefix key, for major-mode specific commands.

This needs to be changed from $SEADIR/init.el.")

(defvar sea-localleader-alt-key "M-SPC m"
  "The localleader prefix key, for major-mode specific commands. Used for Insert
and Emacs states, and for non-evil users.

This needs to be changed from $SEADIR/init.el.")

(defvar sea-leader-map (make-sparse-keymap)
  "An overriding keymap for <leader> keys.")


;;
;;; Universal, non-nuclear escape

;; `keyboard-quit' is too much of a nuclear option. I wanted an ESC/C-g to
;; do-what-I-mean. It serves four purposes (in order):
;;
;; 1. Quit active states; e.g. highlights, searches, snippets, iedit,
;;    multiple-cursors, recording macros, etc.
;; 2. Close popup windows remotely (if it is allowed to)
;; 3. Refresh buffer indicators, like git-gutter and flycheck
;; 4. Or fall back to `keyboard-quit'
;;
;; And it should do these things incrementally, rather than all at once. And it
;; shouldn't interfere with recording macros or the minibuffer. This may require
;; you press ESC/C-g two or three times on some occasions to reach
;; `keyboard-quit', but this is much more intuitive.

(defvar sea-escape-hook nil
  "A hook run after C-g is pressed (or ESC in normal mode, for evil users). Both
trigger `sea/escape'.

If any hook returns non-nil, all hooks after it are ignored.")

(defun sea/escape ()
  "Run `sea-escape-hook'."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
	 ;; quit the minibuffer if open.
	 (abort-recursive-edit))
	;; Run all escape hooks. If any returns non-nil, then stop there.
	((run-hook-with-args-until-success 'sea-escape-hook))
	;; don't abort macros
	((or defining-kbd-macro executing-kbd-macro) nil)
	;; Back to the default
	((keyboard-quit))))

(global-set-key [remap keyboard-quit] #'sea/escape)


;;
;;; General + leader/localleader keys

(require 'general)
;; Convenience aliases
(defalias 'define-key! #'general-def)
(defalias 'unmap! #'general-unbind)

(defmacro after! (targets &rest body)
  "Evaluate BODY after TARGETS (packages) have loaded.

This is a wrapper around `with-eval-after-load' that:

1. Suppresses warnings for disabled packages at compile-time
2. No-ops for TARGETS that are disabled by the user (via `package!')
3. Supports compound TARGETS statements (see below)

TARGETS is a list of packages in one of these formats:

- An unquoted package symbol (the name of a package)
    (after! helm BODY...)
- An unquoted list of package symbols (i.e. BODY is evaluated once both magit
  and git-gutter have loaded)
    (after! (magit git-gutter) BODY...)
- An unquoted, nested list of compound package lists, using :or/:any and/or
  :and/:all
    (after! (:or package-a package-b ...)  BODY...)
    (after! (:and package-a package-b ...) BODY...)
    (after! (:and package-a (:or package-b package-c) ...) BODY...)

Note that:
- :or and :any are equivalent
- :and and :all are equivalent
- If these are omitted, :and is implied."
  (declare (indent defun) (debug t))
  (unless (and (symbolp targets)
	       (memq targets (bound-and-true-p sea-disabled-packages)))
    (list (if (or (not (bound-and-true-p byte-compile-current-file))
		  (dolist (next (sea-enlist targets))
		    (unless (keywordp next)
		      (if (symbolp next)
			  (require next nil :no-error)
			(load next :no-message :no-error)))))
	      #'progn
	    #'with-no-warnings)
	  (if (symbolp targets)
	      `(with-eval-after-load ',targets ,@body)
	    (pcase (car-safe targets)
	      ((or :or :any)
	       (macroexp-progn
		(cl-loop for next in (cdr targets)
			 collect `(after! ,next ,@body))))
	      ((or :and :all)
	       (dolist (next (cdr targets))
		 (setq body `((after! ,next ,@body))))
	       (car body))
	      (_ `(after! (:and ,@targets) ,@body)))))))

;; `map!' uses this instead of `define-leader-key!' because it consumes 20-30%
;; more startup time, so we reimplement it ourselves.
(defmacro sea--define-leader-key (&rest keys)
  (let (prefix forms wkforms)
    (while keys
      (let ((key (pop keys))
	    (def (pop keys)))
	(if (keywordp key)
	    (when (memq key '(:prefix :infix))
	      (setq prefix def))
	  (when prefix
	    (setq key `(general--concat t ,prefix ,key)))
	  (let* ((udef (cdr-safe (sea-unquote def)))
		 (bdef (if (general--extended-def-p udef)
			   (general--extract-def (general--normalize-extended-def udef))
			 def)))
	    (unless (eq bdef :ignore)
	      (push `(define-key sea-leader-map (general--kbd ,key)
		       ,bdef)
		    forms))
	    (when-let (desc (cadr (memq :which-key udef)))
	      (push `(which-key-add-key-based-replacements
		       (general--concat t sea-leader-alt-key ,key)
		       ,desc)
		    wkforms)
	      (push `(which-key-add-key-based-replacements
		       (general--concat t sea-leader-key ,key)
		       ,desc)
		    wkforms))))))
    (macroexp-progn
     (append (nreverse forms)
	     (when wkforms
	       `((after! which-key
			 ,@(nreverse wkforms))))))))

(defmacro define-leader-key! (&rest args)
  "Define <leader> keys.

Uses `general-define-key' under the hood, but does not support :states,
:wk-full-keys or :keymaps. Use `map!' for a more convenient interface.

See `sea-leader-key' and `sea-leader-alt-key' to change the leader prefix."
  `(general-define-key
    :states nil
    :wk-full-keys nil
    :keymaps 'sea-leader-map
    ,@args))

(defmacro define-localleader-key! (&rest args)
  "Define <localleader> key.

Uses `general-define-key' under the hood, but does not support :major-modes,
:states, :prefix or :non-normal-prefix. Use `map!' for a more convenient
interface.

See `sea-localleader-key' and `sea-localleader-alt-key' to change the
localleader prefix."
  (if (featurep 'evil)
      ;; :non-normal-prefix doesn't apply to non-evil sessions (only evil's
      ;; emacs state)
      `(general-define-key
	:states '(normal visual motion emacs insert)
	:major-modes t
	:prefix sea-localleader-key
	:non-normal-prefix sea-localleader-alt-key
	,@args)
    `(general-define-key
      :major-modes t
      :prefix sea-localleader-alt-key
      ,@args)))

;; We use a prefix commands instead of general's :prefix/:non-normal-prefix
;; properties because general is incredibly slow binding keys en mass with them
;; in conjunction with :states -- an effective doubling of Sea's startup time!
(define-prefix-command 'sea/leader 'sea-leader-map)
(define-key sea-leader-map [override-state] 'all)

;; Bind `sea-leader-key' and `sea-leader-alt-key' as late as possible to give
;; the user a chance to modify them.
(defun sea|init-leader-keys ()
  "Bind `sea-leader-key' and `sea-leader-alt-key'."
  (let ((map general-override-mode-map))
    (if (not (featurep 'evil))
	(progn
	  (cond ((equal sea-leader-alt-key "C-c")
		 (set-keymap-parent sea-leader-map mode-specific-map))
		((equal sea-leader-alt-key "C-x")
		 (set-keymap-parent sea-leader-map ctl-x-map)))
	  (define-key map (kbd sea-leader-alt-key) 'sea/leader))
      (evil-define-key* '(normal visual motion) map (kbd sea-leader-key) 'sea/leader)
      (evil-define-key* '(emacs insert) map (kbd sea-leader-alt-key) 'sea/leader))
    (general-override-mode +1)))
(add-hook 'sea-after-init-modules-hook #'sea|init-leader-keys)


;;
;;; Packages



;;;###package hydra
(setq lv-use-seperator t)


;;
;;; `map!' macro

(defvar sea-evil-state-alist
  '((?n . normal)
    (?v . visual)
    (?i . insert)
    (?e . emacs)
    (?o . operator)
    (?m . motion)
    (?r . replace)
    (?g . global))
  "A list of cons cells that map a letter to a evil state symbol.")

(defun sea--keyword-to-states (keyword)
  "Convert a KEYWORD into a list of evil state symbols.

For example, :nvi will map to (list 'normal 'visual 'insert). See
`sea-evil-state-alist' to customize this."
  (cl-loop for l across (substring (symbol-name keyword) 1)
	   if (cdr (assq l sea-evil-state-alist)) collect it
	   else do (error "not a valid state: %s" l)))


;; Register keywords for proper indentation (see `map!')
(put :after        'lisp-indent-function 'defun)
(put :desc         'lisp-indent-function 'defun)
(put :leader       'lisp-indent-function 'defun)
(put :localleader  'lisp-indent-function 'defun)
(put :map          'lisp-indent-function 'defun)
(put :keymap       'lisp-indent-function 'defun)
(put :mode         'lisp-indent-function 'defun)
(put :prefix       'lisp-indent-function 'defun)
(put :prefix-map   'lisp-indent-function 'defun)
(put :unless       'lisp-indent-function 'defun)
(put :when         'lisp-indent-function 'defun)

;; specials
(defvar sea--map-forms nil)
(defvar sea--map-fn nil)
(defvar sea--map-batch-forms nil)
(defvar sea--map-state '(:dummy t))
(defvar sea--map-parent-state nil)
(defvar sea--map-evil-p nil)
(after! evil (setq sea--map-evil-p t))

(defun sea--map-process (rest)
  (let ((sea--map-fn sea--map-fn)
	sea--map-state
	sea--map-forms
	desc)
    (while rest
      (let ((key (pop rest)))
	(cond ((listp key)
	       (sea--map-nested nil key))

	      ((keywordp key)
	       (pcase key
		 (:leader
		  (sea--map-commit)
		  (setq sea--map-fn 'sea--define-leader-key))
		 (:localleader
		  (sea--map-commit)
		  (setq sea--map-fn 'define-localleader-key!))
		 (:after
		  (sea--map-nested (list 'after! (pop rest)) rest)
		  (setq rest nil))
		 (:desc
		  (setq desc (pop rest)))
		 ((or :map :map* :keymap)
		  (sea--map-set :keymaps `(quote ,(sea-enlist (pop rest)))))
		 (:mode
		  (push (cl-loop for m in (sea-enlist (pop rest))
				 collect (intern (concat (symbol-name m) "-map")))
			rest)
		  (push :map rest))
		 ((or :when :unless)
		  (sea--map-nested (list (intern (sea-keyword-name key)) (pop rest)) rest)
		  (setq rest nil))
		 (:prefix-map
		  (cl-destructuring-bind (prefix . desc)
		      (sea-enlist (pop rest))
		    (let ((keymap (intern (format "sea-leader-%s-map" desc))))
		      (setq rest
			    (append (list :desc desc prefix keymap
					  :prefix prefix)
				    rest))
		      (push `(defvar ,keymap (make-sparse-keymap))
			    sea--map-forms))))
		 (:prefix
		  (cl-destructuring-bind (prefix . desc)
		      (sea-enlist (pop rest))
		    (sea--map-set (if sea--map-fn :infix :prefix)
				   prefix)
		    (when (stringp desc)
		      (setq rest (append (list :desc desc "" nil) rest)))))
		 (:textobj
		  (let* ((key (pop rest))
			 (inner (pop rest))
			 (outer (pop rest)))
		    (push `(map! (:map evil-inner-text-objects-map ,key ,inner)
				 (:map evil-outer-text-objects-map ,key ,outer))
			  sea--map-forms)))
		 (_
		  (condition-case _
		      (sea--map-def (pop rest) (pop rest) (sea--keyword-to-states key) desc)
		    (error
		     (error "Not a valid `map!' property: %s" key)))
		  (setq desc nil))))

	      ((sea--map-def key (pop rest) nil desc)
	       (setq desc nil)))))

    (sea--map-commit)
    (macroexp-progn (nreverse (delq nil sea--map-forms)))))

(defun sea--map-append-keys (prop)
  (let ((a (plist-get sea--map-parent-state prop))
	(b (plist-get sea--map-state prop)))
    (if (and a b)
	`(general--concat nil ,a ,b)
      (or a b))))

(defun sea--map-nested (wrapper rest)
  (sea--map-commit)
  (let ((sea--map-parent-state (sea--map-state)))
    (push (if wrapper
	      (append wrapper (list (sea--map-process rest)))
	    (sea--map-process rest))
	  sea--map-forms)))

(defun sea--map-set (prop &optional value)
  (unless (equal (plist-get sea--map-state prop) value)
    (sea--map-commit))
  (setq sea--map-state (plist-put sea--map-state prop value)))

(defun sea--map-def (key def &optional states desc)
  (when (or (memq 'global states)
	    (null states))
    (setq states (cons 'nil (delq 'global states))))
  (when desc
    (let (unquoted)
      (cond ((and (listp def)
		  (keywordp (car-safe (setq unquoted (sea-unquote def)))))
	     (setq def (list 'quote (plist-put unquoted :which-key desc))))
	    ((setq def (cons 'list
			     (if (and (equal key "")
				      (null def))
				 `(:ignore t :which-key ,desc)
			       (plist-put (general--normalize-extended-def def)
					  :which-key desc))))))))
  (dolist (state states)
    (push (list key def)
	  (alist-get state sea--map-batch-forms)))
  t)

(defun sea--map-commit ()
  (when sea--map-batch-forms
    (cl-loop with attrs = (sea--map-state)
	     for (state . defs) in sea--map-batch-forms
	     if (or sea--map-evil-p (not state))
	     collect `(,(or sea--map-fn 'general-define-key)
		       ,@(if state `(:states ',state)) ,@attrs
		       ,@(mapcan #'identity (nreverse defs)))
	     into forms
	     finally do (push (macroexp-progn forms) sea--map-forms))
    (setq sea--map-batch-forms nil)))

(defun sea--map-state ()
  (let ((plist
	 (append (list :prefix (sea--map-append-keys :prefix)
		       :infix  (sea--map-append-keys :infix)
		       :keymaps
		       (append (plist-get sea--map-parent-state :keymaps)
			       (plist-get sea--map-state :keymaps)))
		 sea--map-state
		 nil))
	newplist)
    (while plist
      (let ((key (pop plist))
	    (val (pop plist)))
	(when (and val (not (plist-member newplist key)))
	  (push val newplist)
	  (push key newplist))))
    newplist))

;;
(defmacro map! (&rest rest)
  "A convenience macro for defining keybinds, powered by `general'.

If evil isn't loaded, evil-specific bindings are ignored.

States
  :n  normal
  :v  visual
  :i  insert
  :e  emacs
  :o  operator
  :m  motion
  :r  replace
  :g  global  (binds the key without evil `current-global-map')

  These can be combined in any order, e.g. :nvi will apply to normal, visual and
  insert mode. The state resets after the following key=>def pair. If states are
  omitted the keybind will be global (no emacs state; this is different from
  evil's Emacs state and will work in the absence of `evil-mode').

Properties
  :leader [...]                   an alias for (:prefix sea-leader-key ...)
  :localleader [...]              bind to localleader; requires a keymap
  :mode [MODE(s)] [...]           inner keybinds are applied to major MODE(s)
  :map [KEYMAP(s)] [...]          inner keybinds are applied to KEYMAP(S)
  :keymap [KEYMAP(s)] [...]       same as :map
  :prefix [PREFIX] [...]          set keybind prefix for following keys. PREFIX
				  can be a cons cell: (PREFIX . DESCRIPTION)
  :prefix-map [PREFIX] [...]      same as :prefix, but defines a prefix keymap
				  where the following keys will be bound. DO NOT
				  USE THIS IN YOUR PRIVATE CONFIG.
  :after [FEATURE] [...]          apply keybinds when [FEATURE] loads
  :textobj KEY INNER-FN OUTER-FN  define a text object keybind pair
  :if [CONDITION] [...]
  :when [CONDITION] [...]
  :unless [CONDITION] [...]

  Any of the above properties may be nested, so that they only apply to a
  certain group of keybinds.

Example
  (map! :map magit-mode-map
	:m  \"C-r\" 'do-something           ; C-r in motion state
	:nv \"q\" 'magit-mode-quit-window   ; q in normal+visual states
	\"C-x C-r\" 'a-global-keybind
	:g \"C-x C-r\" 'another-global-keybind  ; same as above

	(:when IS-MAC
	 :n \"M-s\" 'some-fn
	 :i \"M-o\" (lambda (interactive) (message \"Hi\"))))"
  (sea--map-process rest))

(provide 'core-keybinds)
;;; core-keybinds.el ends here
