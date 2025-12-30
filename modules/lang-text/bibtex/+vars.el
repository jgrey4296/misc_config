;;; domain-specific/bibtex/+vars.el -*- lexical-binding: t; -*-

(defvar jg-bibtex-mode-map (make-sparse-keymap))

(defvar jg-bibtex-helm-candidates nil)

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

;;-- general bibtex settings
(setq-default bibtex-user-optional-fields          nil
              bibtex-completion-bibliography       nil
              bibtex-sort-entry-class              '(("String") ("Book" "Proceedings" "book" "proceedings") ("article" "inbook") ("online") (catch-all))
              bibtex-field-indentation             1
              bibtex-text-indentation              15
              bibtex-align-at-equal-sign           t
              org-bibtex-export-arbitrary-fields   t
              )
;;-- end general bibtex settings

;;-- hl line
(after! hl-line
  (add-to-list 'global-hl-line-modes 'bibtex-mode)
  )
;;-- end hl line

;;-- librarian
(speckler-setq! bibtex ()
  librarian--biblio-edit-todo-loc        (expand-file-name "~/.config/bibliography/in_progress/todo.bib")
  librarian--biblio-edit-todo-files-loc  (expand-file-name "/media/john/data/todo/pdfs/")
  librarian--biblio-edit-completions-loc (expand-file-name "~/.config/bibliography/completions/")
  librarian--biblio-edit-export-bib-loc  (expand-file-name "tex-config/tex/export_template.tex" templates-loc)
  librarian--biblio-edit-temp-tex-loc    (expand-file-name ".tex/" user-cache-dir)
  librarian--biblio-pdf-loc (pcase system-type
                             ('darwin (expand-file-name "~/pdf_library"))
                             ('gnu/linux "/media/john/data/library/pdfs_structured"))
  librarian-biblio-library-loc   (expand-file-name "~/.config/bibliography/main/")
  librarian-biblio-unsourced-loc (expand-file-name "~/.config/bibliography/in_progress/to_source_misc.bib")
  org-ref-clean-bibtex-entry-hook librarian--biblio-clean-hooks

  librarian--biblio-edit-search-fields '("tags" "year" "publisher")
  bibtex-completion-additional-search-fields librarian--biblio-edit-search-fields
  bibtex-completion-pdf-field                "file"
  bibtex-completion-display-formats jg-bibtex-completion-display-formats
  bibtex-completion-pdf-open-function 'browse-url
  )

;;-- end librarian

;;-- specs
(speckler-add! online-search ()
  '(bibtex
    ("Scholar"           "https://scholar.google.com/scholar?hl=en&q=%s")
    ("Scholar Archive"   "https://scholar.archive.org/search?q=%s")
    ("DBLP"              "https://dblp1.uni-trier.de/search?q=%s")
    ("Doi"               "https://doi.org/%s")
    ("Archive.org"       "https://archive.org/search.php?query=%s")
    ("Project Gutenberg" "http://www.gutenberg.org/ebooks/search/?query=%s")
    ("Amazon UK"         "https://www.amazon.co.uk/s?k=%s")
    ("Amazon US"         "https://www.amazon.com/s?k=%s")
    ("IMDB"              "https://www.imdb.com/find?s=all&q=%s")
    ("ORCID"             "https://orcid.org/orcid-search/search?firstName=%s&otherFields=true")
    ("Arxiv"             "https://arxiv.org/abs/%s")
    )
  )
(speckler-add! whitespace-cleanup ()
  '(bibtex-mode
    #'librarian--biblio-clean-ensure-newline-before-def
    #'delete-trailing-whitespace
    #'+jg-text-cleanup-whitespace
    )
  )
(speckler-add! auto-modes ()
  '(bibtex
    ("\\.bib\\'" . bibtex-mode)
    ("\\.bst\\'" . bibtex-style-mode)
    ("\\.bbx\\'" . latex-mode)
    ("\\.cbx\\'" . latex-mode)
    )
  )
(speckler-add! popup ()
  '(bibtex
    ("^\\* DOI Metadata\\*\\'"     :side left :ttl 5 :width 0.3 :quit t :select nil :priority 50)
    ("^\\* Metadata\\*\\'"         :side left :ttl 5 :width 0.3 :quit t :select nil :priority 50)
    ("^\\* Url Metadata\\*\\'"     :side bottom :ttl 5 :width 0.2 :quit t :select nil :priority 50)
    )
  )
(speckler-add! treesit-source ()
  '(bibtex        "git@github.com:latex-lsp/tree-sitter-bibtex.git")
  )
(speckler-add! compile-commands ()
  '(bibtex #'+jg-bibtex-get-commands)
  )
;;-- end specs
