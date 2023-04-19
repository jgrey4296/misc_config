;;; domain-specific/bibtex/+vars.el -*- lexical-binding: t; -*-

;;-- personal settings
(setq-default jg-bibtex-indent-equals-column 14
              jg-bibtex-fill-column 50000

              jg-bibtex-helm-candidates nil
              jg-bibtex-candidates-names '()

              jg-bibtex-rand-log ".emacs_rand_bib_log"
              jg-bibtex-tweet-rand-log ".emacs_tweet_rand_bib_log"

              jg-bibtex-clean-move-entry-on-fail nil
              jg-bibtex-open-doi-with-pdf        nil
              jg-bibtex-open-url-with-pdf        nil
              )

;;-- end personal settings

;;-- general bibtex settings
(setq-default bibtex-user-optional-fields          nil
              bibtex-completion-bibliography       nil
              bibtex-field-indentation             1
              bibtex-text-indentation              15
              bibtex-align-at-equal-sign           t
              org-bibtex-export-arbitrary-fields   t
              )
;;-- end general bibtex settings

;;-- bibtex fields
(setq jg-bibtex-search-fields               '("tags" "year" "publisher")
      jg-bibtex-scholar-search-fields       '("author" "editor" "ALTauthor" "Alteditor" "year" "doi" "isbn")
      jg-bibtex-scholar-search-fields-exact '("title")
      )
(setq bibtex-completion-additional-search-fields jg-bibtex-search-fields)
(setq bibtex-completion-pdf-field "file")
;;-- end bibtex fields

;;-- urls
(setq jg-bibtex-reference-url         "https://www.bibtex.com/g/bibtex-format/"
      jg-bibtex-doi-url               "https://doi.org/%s"
      )
;;-- end urls

;;-- strings
(setq jg-bibtex-completion-display-formats
      '(
        (t . ("${author:20} || ${title:*} || ${year:4}" 40))
        )
      )
(setq jg-bibtex-pdf-loc-regexp               "file[[:digit:]]*\s*=\s*{\\(.+\\)/\\(.+pdf_library\\)?"
      jg-bibtex-pdf-replace-match-string     "~/"
      jg-bibtex-pdf-replace-library-string   "pdf_library"
      jg-bibtex-remove-field-newlines-regexp "^="
      jg-bibtex-tweet-pattern                "(%s): %s\nby %s\nTags: %s\nRef: %s\n"
 )
;;-- end strings

;;-- locations
(setq-default jg-bibtex-loc-bibtex          (expand-file-name "~/github/jgrey4296.github.io/resources/bibliography/")
              jg-bibtex-todo-loc            (expand-file-name "~/github/jgrey4296.github.io/resources/todo.bib")
              jg-bibtex-loc-completions     (expand-file-name "~/github/jgrey4296.github.io/resources/completions/")
              jg-bibtex-loc-export-bib-file (doom-module-expand-path :lang-text 'bibtex "_data/export_template.tex")
              jg-bibtex-loc-temp-dir        (expand-file-name "~/.tex/")
              jg-bibtex-pdf-loc             (expand-file-name "~/pdf_library")
 )
;;-- end locations

;;-- commands
(setq jg-bibtex-curl-cmd      "curl"
      jg-bibtex-curl-args     "-sLI"
      ;; bibtex-completion-pdf-open-function #'(lambda (x) (org-open-link-from-string (format "[[file:%s]]" x)))
      )
(defvar bibtex-completion-pdf-open-function 'browse-url)
;;-- end commands

;;-- hl line
(after! hl-line
  (add-to-list 'global-hl-line-modes 'bibtex-mode)
  )
;;-- end hl line

;;-- specs
(spec-handling-add! lookup-url nil
                    ('bibtex
                     ("Scholar"  "https://scholar.google.com/scholar?hl=en&q=%s")
                     ("Scholar Archive"    "https://scholar.archive.org/search?q=%s")
                     ("DBLP"               "https://dblp1.uni-trier.de/search?q=%s")
                     ("Doi"                "https://doi.org/%s")
                     ("Wikipedia"          "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
                     ("Archive.org"        "https://archive.org/search.php?query=%s")
                     ("Project Gutenberg"  "http://www.gutenberg.org/ebooks/search/?query=%s")
                     ("Amazon UK"          "https://www.amazon.co.uk/s?k=%s")
                     ("Amazon US"          "https://www.amazon.com/s?k=%s")
                     ("IMDB"               "https://www.imdb.com/find?s=all&q=%s")
                     )
                    )

(spec-handling-add! tagging nil
                    (bibtex-mode
                     :set  +jg-bibtex-set-tags
                     :new  +jg-bibtex-set-new-tag
                     :get  +jg-bibtex-get-tags
                     )
                    )

;;-- end specs
