(setq-default tag-unify/loc-bookmarks "~/github/writing/resources"
              tag-unify/loc-bibtex "~/github/writing/resources/years"
              tag-unify/loc-master-tag-list ""
              bibtex-completion-bibliography nil
              bibtex-completion-additional-search-fields '("tags" "year")
              bibtex-completion-pdf-field "file"
              bibtex-completion-pdf-open-function (lambda (x) (org-open-link-from-string (format "[[file:%s]]" x)))
              tag-unify/helm-bibtex-candidates nil
              )
