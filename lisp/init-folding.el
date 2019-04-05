;; init-folding.el
(use-package origami
  ;; :quelpa (origami :repo "seblemaguer/origami.el" :fetcher github)
  :custom
  (origami-show-fold-header t)

  :custom-face
  (origami-fold-replacement-face ((t (:inherit magit-diff-context-highlight))))
  (origami-fold-fringe-face ((t (:inherit magit-diff-context-highlight))))

  :init
  (global-origami-mode)
  (defhydra origami-hydra (:color blue :hint none)
    "
      _:_: recursively toggle node       _a_: toggle all nodes    _t_: toggle node
      _o_: show only current node        _u_: undo                _r_: redo
      _R_: reset
      "
    (":" origami-recursively-toggle-node)
    ("a" origami-toggle-all-nodes)
    ("t" origami-toggle-node)
    ("o" origami-show-only-node)
    ("u" origami-undo)
    ("r" origami-redo)
    ("R" origami-reset))

  :bind
  ("C-:" . origami-hydra/body)
  ("C-{" . origami-recursively-toggle-node)
  :config
  (face-spec-reset-face 'origami-fold-header-face))


(provide 'init-folding)
