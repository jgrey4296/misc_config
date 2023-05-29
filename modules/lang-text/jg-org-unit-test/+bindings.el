;;; util/jg-org-unit-test/+bindings.el -*- lexical-binding: t; -*-

(doom-log "Setting up org unit test bindings: %s" (current-time-string))

(defvar jg-org-unit-test-map (make-sparse-keymap))
(define-minor-mode jg-org-unit-test-minor-mode
  "  "
  :lighter "org-unit-test"
  :keymap jg-org-unit-test-map
  )
(map! :map jg-org-unit-test-map
      :localleader
      :prefix "."
      :desc "Run Org Test" "T" #'+jg-org-test-org-file
      )
