;;; util/jg-tag/+org-spec-bindings.el -*- lexical-binding: t; -*-

(map! :map org-mode-map
      :localleader
      :prefix ("f" . "Format")
      :desc "Clean Tags"          "c" #'jg-tag-clean-org
      :desc "Wrap Numbers"        "w" #'jg-tag-wrap-numbers
      :desc "Wrap non-link urls"  "L" #'jg-tag-wrap-non-link-urls
      :desc "Remove Duplicates"   "D" #'jg-tag-remove-duplicates
      )
