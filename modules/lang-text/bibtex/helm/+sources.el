;;; +sources.el -*- lexical-binding: t; -*-

  ;; Define the bib helm
(require 'helm-source)

(setq jg-bibtex-helm-source-bibtex
      (helm-build-sync-source "Bibtex Helm"
        :action (helm-make-actions  "Insert citation"      #'helm-bibtex-insert-citation
                                "Open file"            #'+jg-bibtex-helm-open-files
                                "Insert BibTeX key"    #'helm-bibtex-insert-key
                                "Insert BibTeX entry"  #'helm-bibtex-insert-bibtex
                                "Insert Bibtex simple" #'+jg-bibtex-insert-simple
                                "Show entry"           #'+jg-bibtex-show-entry
                                "Edit Notes"           #'+jg-bibtex-edit-notes
                                "Tweet Entry"          #'+jg-bibtex-helm-tweet-action
                                )
        :candidates 'helm-bibtex-candidates
        :filtered-candidate-transformer  '(+jg-bibtex-year-sort-transformer
                                        +jg-bibtex-helm-candidates-formatter
                                        helm-fuzzy-highlight-matches)
        :multimatch
        :fuzzy-match
        )
)
