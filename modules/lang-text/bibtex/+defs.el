;; +defs.el -*- lexical-binding: t; -*-

(defvar jg-bibtex-indent-equals-column 14)

(defvar jg-bibtex-fill-column 50000)

(defvar jg-bibtex-helm-candidates nil)

(defvar jg-bibtex-candidates-names '())

(defvar jg-bibtex-rand-log ".emacs_rand_bib_log")

(defvar jg-bibtex-clean-move-entry-on-fail nil)

(defvar jg-bibtex-open-doi-with-pdf        nil)

(defvar jg-bibtex-open-url-with-pdf        nil)

(defvar jg-bibtex-todo-loc            (expand-file-name "~/github/bibliography/in_progress/todo.bib"))

(defvar jg-bibtex-loc-completions     (expand-file-name "~/github/bibliography/completions/"))

(defvar jg-bibtex-loc-export-bib-file (expand-file-name "tex-config/tex/export_template.tex" templates-loc))

(defvar jg-bibtex-loc-temp-dir        (expand-file-name "~/.tex/"))

(defvar jg-bibtex-in-progress-files-locs (expand-file-name "/media/john/data/todo/pdfs/"))

(defvar jg-bibtex-search-fields               '("tags" "year" "publisher"))

(defvar jg-bibtex-scholar-search-fields       '("author" "editor" "ALTauthor" "Alteditor" "year" "doi" "isbn"))

(defvar jg-bibtex-scholar-search-fields-exact '("title"))

(defvar jg-bibtex-doi-url               "https://doi.org/%s")

(defvar jg-bibtex-completion-display-formats
      '(
        (judicial . "${=has-pdf=:1} ${=type=:10} || ${year:4} || ${author:20} || ${short_parties:80} || ${tags:*}")
        (review   . "${=has-pdf=:1} REVIEW     || ${year:4} || ${author:20} || REVIEW ${title:73} || ${tags:*}")
        (online   . "${=has-pdf=:1} ${=type=:10} || ${year:4} || ${author:20} || ${title:80} || ${tags:*}")
        (t        . "${=has-pdf=:1} ${=type=:10} || ${year:4} || ${author:20} || ${title:80} || ${tags:*}")
        ;; (t     . ("${author:20} || ${title:*} || ${year:4}" 40))
        ;; (t     . "${author:35} ${title:*} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7}")
        )
      "Display formats for the ivy bibtex. see bibtex-completion-display-formats"
      )

(setq bibtex-completion-display-formats jg-bibtex-completion-display-formats)

(defvar jg-bibtex-pdf-loc-regexp               (format "file[[:digit:]]*\s*=\s*{\\(.+\\)/\\(.+%s\\)?"
                                                       (pcase system-type
                                                         ('darwin "pdf_library")
                                                         ('gnu/linux "pdfs")
                                                         )))

(defvar jg-bibtex-pdf-replace-match-string     "~/")

(defvar jg-bibtex-pdf-replace-library-string   (pcase system-type
                                                 ('darwin "pdf_library")
                                                 ('gnu/linux "pdfs")))

(defvar jg-bibtex-remove-field-newlines-regexp "^=")

(defvar jg-bibtex-curl-cmd      "curl")

(defvar jg-bibtex-curl-args     '("-sLI" "--connect-timeout" "3"))

(defvar bibtex-completion-pdf-open-function 'browse-url)

(defvar jg-bibtex-clean-hooks '(+jg-bibtex-insert-stub-key ;; Initial key
                                ;; Initial formatting
                                +jg-bibtex-remove-empty-fields
                                +jg-bibtex-dont-break-lines-hook
                                ;; +jg-bibtex-latex-normalise - shifted to doot
                                +jg-bibtex-normalise-symbols

                                ;; Specific fields
                                ;; org-ref-title-case-article
                                +jg-bibtex-clean-doi-hook
                                +jg-bibtex-check-file-hook
                                +jg-bibtex--expand-shortened-url
                                ;; +jg-bibtex-isbn-clean - shifted to doot task
                                ;; generate key
                                +jg-bibtex-orcb-key-hook
                                +jg-bibtex-insert-volume-to-key
                                ;; Final alignment and indent
                                +jg-bibtex-clean-whitespace-hook
                                +jg-bibtex-align-hook
                                +jg-bibtex-indent-hook
                              )
      )
