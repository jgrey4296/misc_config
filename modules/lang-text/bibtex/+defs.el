;; +defs.el -*- lexical-binding: t; -*-

(defvar jg-bibtex-indent-equals-column 14)

(defvar jg-bibtex-fill-column 50000)

(defvar jg-bibtex-helm-candidates nil)

(defvar jg-bibtex-candidates-names '())

(defvar jg-bibtex-rand-log ".emacs_rand_bib_log")

(defvar jg-bibtex-tweet-rand-log ".emacs_tweet_rand_bib_log")

(defvar jg-bibtex-clean-move-entry-on-fail nil)

(defvar jg-bibtex-open-doi-with-pdf        nil)

(defvar jg-bibtex-open-url-with-pdf        nil)

(defvar jg-bibtex-loc-bibtex          (expand-file-name "~/github/jgrey4296.github.io/resources/bibliography/"))

(defvar jg-bibtex-todo-loc            (expand-file-name "~/github/jgrey4296.github.io/resources/todo.bib"))

(defvar jg-bibtex-loc-completions     (expand-file-name "~/github/jgrey4296.github.io/resources/completions/"))

(defvar jg-bibtex-loc-export-bib-file (expand-file-name "tex/export_template.tex" "~/.doom.d/terminal/tex-config/"))

(defvar jg-bibtex-loc-temp-dir        (expand-file-name "~/.tex/"))

(defvar jg-bibtex-pdf-loc             (expand-file-name "~/pdf_library"))

(defvar jg-bibtex-unsourced-bib-file (expand-file-name "~/github/jgrey4296.github.io/resources/to_source.bib"))

(defvar jg-bibtex-search-fields               '("tags" "year" "publisher"))

(defvar jg-bibtex-scholar-search-fields       '("author" "editor" "ALTauthor" "Alteditor" "year" "doi" "isbn"))

(defvar jg-bibtex-scholar-search-fields-exact '("title"))

(defvar jg-bibtex-doi-url               "https://doi.org/%s")

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

(defvar jg-bibtex-curl-cmd      "curl")

(defvar jg-bibtex-curl-args     "-sLI")

(defvar bibtex-completion-pdf-open-function 'browse-url)

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