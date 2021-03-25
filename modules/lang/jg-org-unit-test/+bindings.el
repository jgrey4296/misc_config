;;; util/jg-org-unit-test/+bindings.el -*- lexical-binding: t; -*-


(map! :after org
      :map org-mode-map
      :localleader
      :prefix "."
      :desc "Run Org Test" "T" #'jg-org-test-test-org-file
)
