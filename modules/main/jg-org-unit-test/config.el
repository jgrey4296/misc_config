;;; specific/org-unit-test-layer/config.el -*- lexical-binding: t; -*-

(after! org
  (map! :leader
        "a u T" 'jg-org-test-test-org-file)
  )

(load! "+funcs")
(load! "+test-retrieval")
(load! "+test-parsing")
(load! "+test-execution")
(load! "+test-reporting")
