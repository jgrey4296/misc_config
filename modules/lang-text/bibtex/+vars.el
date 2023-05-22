;;; domain-specific/bibtex/+vars.el -*- lexical-binding: t; -*-

;;-- personal settings

(defvar jg-bibtex-indent-equals-column 14)

(defvar jg-bibtex-fill-column 50000)

(defvar jg-bibtex-helm-candidates nil)

(defvar jg-bibtex-candidates-names '())

(defvar jg-bibtex-rand-log ".emacs_rand_bib_log")

(defvar jg-bibtex-tweet-rand-log ".emacs_tweet_rand_bib_log")

(defvar jg-bibtex-clean-move-entry-on-fail nil)

(defvar jg-bibtex-open-doi-with-pdf        nil)

(defvar jg-bibtex-open-url-with-pdf        nil)

;;-- end personal settings

;;-- locations

(defvar jg-bibtex-loc-bibtex          (expand-file-name "~/github/jgrey4296.github.io/resources/bibliography/"))

(defvar jg-bibtex-todo-loc            (expand-file-name "~/github/jgrey4296.github.io/resources/todo.bib"))

(defvar jg-bibtex-loc-completions     (expand-file-name "~/github/jgrey4296.github.io/resources/completions/"))

(defvar jg-bibtex-loc-export-bib-file (doom-module-expand-path :lang-text 'bibtex "_data/export_template.tex"))

(defvar jg-bibtex-loc-temp-dir        (expand-file-name "~/.tex/"))

(defvar jg-bibtex-pdf-loc             (expand-file-name "~/pdf_library"))
;;-- end locations

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

(defvar jg-bibtex-search-fields               '("tags" "year" "publisher"))

(defvar jg-bibtex-scholar-search-fields       '("author" "editor" "ALTauthor" "Alteditor" "year" "doi" "isbn"))

(defvar jg-bibtex-scholar-search-fields-exact '("title"))

(setq bibtex-completion-additional-search-fields jg-bibtex-search-fields)
(setq bibtex-completion-pdf-field "file")
;;-- end bibtex fields

;;-- urls

(defvar jg-bibtex-doi-url               "https://doi.org/%s")
;;-- end urls

;;-- strings

(defvar jg-bibtex-completion-display-formats
      '(
        (t . ("${author:20} || ${title:*} || ${year:4}" 40))
        )
      )

(defvar jg-bibtex-pdf-loc-regexp               "file[[:digit:]]*\s*=\s*{\\(.+\\)/\\(.+pdf_library\\)?")

(defvar jg-bibtex-pdf-replace-match-string     "~/")

(defvar jg-bibtex-pdf-replace-library-string   "pdf_library")

(defvar jg-bibtex-remove-field-newlines-regexp "^=")

(defvar jg-bibtex-tweet-pattern                "(%s): %s\nby %s\nTags: %s\nRef: %s\n")
;;-- end strings

;;-- commands

(defvar jg-bibtex-curl-cmd      "curl")

(defvar jg-bibtex-curl-args     "-sLI")
;; bibtex-completion-pdf-open-function #'(lambda (x) (org-open-link-from-string (format "[[file:%s]]" x)))

(defvar bibtex-completion-pdf-open-function 'browse-url)
;;-- end commands

;;-- hl line
(after! hl-line
  (add-to-list 'global-hl-line-modes 'bibtex-mode)
  )
;;-- end hl line

;;-- clean hooks
(defvar jg-bibtex-clean-hooks '(+jg-bibtex-insert-stub-key ;; Initial key
                                ;; Initial formatting
                                +jg-bibtex-remove-empty-fields
                                +jg-bibtex-dont-break-lines-hook
                                +jg-bibtex-smart-replace-nonascii-hook
                                +jg-bibtex-orcb-&

                                ;; Specific fields
                                ;; org-ref-title-case-article
                                +jg-bibtex-clean-doi-hook
                                +jg-bibtex-check-file-hook
                                +jg-bibtex--expand-shortened-url
                                +jg-bibtex-isbn-clean
                                ;; generate key
                                +jg-bibtex-orcb-key-hook
                                +jg-bibtex-insert-volume-to-key
                                ;; Final alignment and indent
                                +jg-bibtex-clean-whitespace-hook
                                +jg-bibtex-align-hook
                                +jg-bibtex-indent-hook
                              )
      )

;;-- end clean hooks

;;-- specs
(spec-handling-add! lookup-url nil
                    '(bibtex
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
                    '(bibtex-mode
                     :set  +jg-bibtex-set-tags
                     :new  +jg-bibtex-set-new-tag
                     :get  +jg-bibtex-get-tags
                     )
                    )
(spec-handling-add! whitespace-cleanup nil
                    '(bibtex-mode
                     +jg-bibtex-cleanup-ensure-newline-before-def
                     delete-trailing-whitespace
                     +jg-text-cleanup-whitespace
                     )
                    )

(spec-handling-add! lookup-regular nil
                    '(bibtex-mode
                     ("bibtex reference" . "https://www.bibtex.com/g/bibtex-format/")
                     )
                    )
;;-- end specs
