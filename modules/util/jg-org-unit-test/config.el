;;; specific/org-unit-test-layer/config.el -*- lexical-binding: t; -*-


(load! "+funcs")
(load! "+test-retrieval")
(load! "+test-parsing")
(load! "+test-execution")
(load! "+test-reporting")

(after! (evil org)
  (defvar org-unit-test-map (make-sparse-keymap))
  (define-minor-mode org-unit-test-minor-mode
    "  "
  :lighter "org-unit-test"
  :keymap org-unit-test-map
  )

  (load! "+bindings")
)
