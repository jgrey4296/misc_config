(setq-default tag-unify/loc-bookmarks "/Users/jgrey/github/writing/resources/main_bookmarks.html"
              tag-unify/loc-bibtex "~/github/writing/resources/years"
              tag-unify/preferred-linecount-for-org 1500
              tag-unify/loc-master-tag-list ""
              tag-unfiy/org-clean-marker nil
              bibtex-completion-bibliography nil
              bibtex-completion-additional-search-fields '("tags" "year")
              bibtex-completion-pdf-field "file"
              bibtex-completion-pdf-open-function (lambda (x) (org-open-link-from-string (format "[[file:%s]]" x)))
              tag-unify/helm-bibtex-candidates nil
              )
