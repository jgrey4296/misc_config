;;; specific/org-unit-test-layer/config.el -*- lexical-binding: t; -*-

(map! :after org
      :map org-mode-map
      :localleader
      :prefix "."
      :desc "Run Org Test" "T" 'jg-org-test-test-org-file
)

(load! "+funcs")
(load! "+test-retrieval")
(load! "+test-parsing")
(load! "+test-execution")
(load! "+test-reporting")
