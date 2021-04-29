;;; specific/org-unit-test-layer/config.el -*- lexical-binding: t; -*-


(load! "+funcs")
(load! "+test-retrieval")
(load! "+test-parsing")
(load! "+test-execution")
(load! "+test-reporting")
(load! "+bindings")

(add-hook! doom-first-input #'+jg-org-unit-test-binding-hook)
