(setq-default tag-unify/loc-bookmarks "~/github/writing/resources/main_bookmarks.html"
              tag-unify/loc-bibtex "~/github/writing/resources/years"
              tag-unify/preferred-linecount-for-org 1500
              tag-unify/loc-master-tag-list ""
              tag-unfiy/org-clean-marker nil

              bibtex-completion-bibliography nil
              bibtex-completion-additional-search-fields '("tags" "year")
              bibtex-completion-pdf-field "file"
              bibtex-completion-pdf-open-function (lambda (x) (org-open-link-from-string (format "[[file:%s]]" x)))
              tag-unify/helm-bibtex-candidates nil

              tag-unify/tag-unify-candidates-names '()
              tag-unify/tag-unify-candidate-counts '()
              ;; Start Position -> End Line number because of changes in positions from tag add/retract
              tag-unify/tag-unify-region '()
              tag-unify/tag-unify-helm `((name . "Helm Tagging")
                                         (action . (("set" . tag-unify/org-set-tags)))
                                         )
              tag-unify/tag-unify-fallback-source `((name . "")
                                                    (action . (("Create" . tag-unify/org-set-new-tag)))
                                                    (filtered-candidate-transformer (lambda (_c _s) (list helm-pattern)))
                                                    )
              )

(add-hook 'bibtex-mode-hook
          (lambda ()
            (let ((misc (assoc "Misc" bibtex-BibTeX-entry-alist))
                  (copied (assoc-delete-all "Misc" (copy-alist bibtex-BibTeX-entry-alist)))
                  (custom '("Misc" "Miscellaneous" nil nil (("author") ("title" "Title of the work (BibTeX converts it to lowercase)") ("howpublished" "The way in which the work was published") ("month") ("year") ("note") ("file") ("keywords")))))
              (setq bibtex-BibTeX-entry-alist (cons custom copied))
              )
            ))
