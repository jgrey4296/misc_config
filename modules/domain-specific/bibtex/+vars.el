;;; domain-specific/bibtex/+vars.el -*- lexical-binding: t; -*-

;;-- bibtex settings
(setq-default bibtex-completion-additional-search-fields '("tags" "year")
              bibtex-completion-bibliography nil
              bibtex-completion-pdf-field "file"
              bibtex-completion-pdf-open-function #'(lambda (x) (org-open-link-from-string (format "[[file:%s]]" x)))
              bibtex-user-optional-fields '(("annotation" "Personal Annotation") ("tags" "Set of tags") ("isbn" "ISBN of file") ("doi" "DOI of file") ("url" "Url of file") ("file" "The path of the file") ("translator" "The Translators of the work"))
              bibtex-field-indentation 1
              bibtex-text-indentation 15
              bibtex-align-at-equal-sign t
)
;;-- end bibtex settings

;;-- personal settings
(setq-default jg-bibtex-indent-equals-column 14
              jg-bibtex-fill-column 50000
              jg-bibtex-clean-add-hooks '(+jg-bibtex-orcb-key-hook +jg-bibtex-smart-replace-nonascii-hook +jg-bibtex-dont-break-lines-hook +jg-bibtex-clean-doi-hook +jg-bibtex-insert-volume-to-key  +jg-bibtex-check-file-hook +jg-bibtex-align-hook +jg-bibtex-indent-hook)
              jg-bibtex-clean-remove-hooks '(org-ref-sort-bibtex-entry orcb-key org-ref-replace-nonascii orcb-clean-doi org-ref-bibtex-format-url-if-doi orcb-check-journal orcb-download-pdf)

              jg-bibtex-scholar-search-fields '("author" "editor" "ALTauthor" "Alteditor" "year" "doi" "isbn")
              jg-bibtex-scholar-search-fields-exact '("title")
              jg-bibtex-scholar-search-string "https://scholar.google.com/scholar?hl=en&q=%s"
              jg-bibtex-dblp-search-string "https://dblp1.uni-trier.de/search?q=%s"


              jg-bibtex-pdf-loc-regexp  "file[[:digit:]]*\s*=\s*{\\(.+mega\\)/\\(.+pdflibrary\\)?"
              jg-bibtex-pdf-replace-match-string "~/Mega"
              jg-bibtex-pdf-replace-library-string "pdflibrary"


              jg-bibtex-helm-candidates nil
              jg-bibtex-candidates-names '()

              jg-bibtex-rand-log ".emacs_rand_bib_log"
              jg-bibtex-tweet-rand-log ".emacs_tweet_rand_bib_log"

              jg-bibtex-remove-field-newlines-regexp "file\\|url\\|title"

              jg-bibtex-clean-move-entry-on-fail nil
              jg-bibtex-open-doi-with-pdf nil
              jg-bibtex-open-url-with-pdf nil

              jg-bibtex-tweet-pattern "(%s): %s\nby %s\nTags: %s\nRef: %s\n"

              jg-bibtex-open-pdf-cmd "open -nF "
              jg-bibtex-open-epub-cmd "open -a ebook-viewer "
              )
;;-- end personal settings

;;-- locations
(setq-default
              jg-bibtex-loc-bibtex          "~/github/writing/resources/bibliography/"
              jg-bibtex-loc-completions     "~/github/writing/resources/completions/"
              jg-bibtex-loc-export-bib-file "~/github/emacs_files/modules/domain-specific/bibtex/export.bib"
              jg-bibtex-loc-temp-dir        "~/.tex/"
              jg-bibtex-pdf-loc             "~/Mega/pdflibrary"
 )
;;-- end locations



;;-- hl line
(after! hl-line
  (push 'bibtex-mode global-hl-line-modes)
  )
;;-- end hl line

;;-- org ebook opening
;;-- end org ebook opening
