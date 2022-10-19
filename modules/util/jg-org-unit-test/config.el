;;; specific/org-unit-test-layer/config.el -*- lexical-binding: t; -*-


(load! "+funcs")
(load! "+test-retrieval")
(load! "+test-parsing")
(load! "+test-execution")
(load! "+test-reporting")

(after! (jg-bindings-total org)
  (defvar jg-org-unit-test-map (make-sparse-keymap))
  (define-minor-mode jg-org-unit-test-minor-mode
    "  "
  :lighter "org-unit-test"
  :keymap jg-org-unit-test-map
  )

  (load! "+bindings")
)
