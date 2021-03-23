;;; main/jg-tag/+vars.el -*- lexical-binding: t; -*-

(setq-default bibtex-completion-additional-search-fields '("tags" "year")
              bibtex-completion-bibliography nil
              bibtex-completion-pdf-field "file"
              bibtex-completion-pdf-open-function #'(lambda (x) (org-open-link-from-string (format "[[file:%s]]" x)))
              bibtex-user-optional-fields '(("annotation" "Personal Annotation") ("tags" "Set of tags") ("isbn" "ISBN of file") ("doi" "DOI of file") ("url" "Url of file") ("file" "The path of the file") ("translator" "The Translators of the work"))

              jg-bibtex-clean-add-hooks '(jg-tag-dont-break-lines-hook jg-orcb-clean-doi jg-bibtex-align)
              jg-bibtex-clean-remove-hooks '(orcb-clean-doi org-ref-bibtex-format-url-if-doi orcb-check-journal)

              jg-scholar-search-fields '("author" "editor" "ALTauthor" "Alteditor" "year" "doi" "isbn")
              jg-scholar-search-fields-exact '("title")
              jg-scholar-search-string "https://scholar.google.com/scholar?hl=en&q=%s"

              jg-tag-bibtex-fill-column 50000
              jg-tag-candidate-counts '()
              jg-tag-candidates-names '()
              jg-tag-global-tags (make-hash-table :test 'equal)
              jg-tag-helm-bibtex-candidates nil
              jg-tag-last-similarity-arg 1

              jg-tag-loc-bibtex                "~/github/writing/resources/bibliography"
              jg-tag-loc-bibtex-completions    "~/github/writing/resources/completions"
              jg-tag-loc-bookmarks             "~/github/writing/resources/main_bookmarks.html"
              jg-tag-loc-global-tags           "~/github/writing/resources/collate.tags"
              jg-tag-loc-twitter-account-index "~/.doom.d/setup_files/tw_acct.index"
              jg-tag-loc-twitter-tag-index     "~/.doom.d/setup_files/tw_tag.index"

              jg-tag-marker (make-marker)
              jg-tag-org-clean-marker nil
              jg-tag-preferred-linecount-for-org 1500
              jg-tag-remove-field-newlines-regexp "file\\|url\\|title"
              jg-tag-twitter-heading-helm-candidates nil
              jg-tag-twitter-helm-candidates nil
              )
