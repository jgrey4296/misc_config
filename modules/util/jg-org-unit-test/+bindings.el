;;; util/jg-org-unit-test/+bindings.el -*- lexical-binding: t; -*-

(message "Setting up org unit test bindings: %s" (current-time-string))
(map! :map org-unit-test-map
      :localleader
      :prefix "."
      :desc "Run Org Test" "T" #'jg-org-test-org-file
      )
