;;; util/jg-org-unit-test/+bindings.el -*- lexical-binding: t; -*-

(message "Setting up org unit test bindings: %s" (current-time-string))
(map! :after org
      :map org-mode-map
      :localleader
      :prefix "."
      :desc "Run Org Test" "T" #'jg-org-test-test-org-file
      )
