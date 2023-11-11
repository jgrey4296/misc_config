;;; domain-specific/bibtex/+vars.el -*- lexical-binding: t; -*-

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
(setq bibtex-completion-additional-search-fields jg-bibtex-search-fields)
(setq bibtex-completion-pdf-field "file")
;;-- end bibtex fields

;;-- hl line
(after! hl-line
  (add-to-list 'global-hl-line-modes 'bibtex-mode)
  )
;;-- end hl line

;;-- specs
(spec-handling-add! lookup-url
                    '(bibtex
                     ("Scholar"           "https://scholar.google.com/scholar?hl=en&q=%s")
                     ("Scholar Archive"   "https://scholar.archive.org/search?q=%s")
                     ("DBLP"              "https://dblp1.uni-trier.de/search?q=%s")
                     ("Doi"               "https://doi.org/%s")
                     ("Wikipedia"         "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
                     ("Archive.org"       "https://archive.org/search.php?query=%s")
                     ("Project Gutenberg" "http://www.gutenberg.org/ebooks/search/?query=%s")
                     ("Amazon UK"         "https://www.amazon.co.uk/s?k=%s")
                     ("Amazon US"         "https://www.amazon.com/s?k=%s")
                     ("IMDB"              "https://www.imdb.com/find?s=all&q=%s")
                     ("ORCID"             "https://orcid.org/orcid-search/search?firstName=%s&otherFields=true")
                     ("Arxiv"             "https://arxiv.org/abs/%s")
                     )
                    )
(spec-handling-add! tagging
                    '(bibtex-mode
                     :set  +jg-bibtex-set-tags
                     :new  +jg-bibtex-set-new-tag
                     :get  +jg-bibtex-get-tags
                     )
                    )
(spec-handling-add! whitespace-cleanup
                    '(bibtex-mode
                     +jg-bibtex-cleanup-ensure-newline-before-def
                     delete-trailing-whitespace
                     +jg-text-cleanup-whitespace
                     )
                    )
(spec-handling-add! lookup-regular
                    '(bibtex-mode
                     ("bibtex reference" . "https://www.bibtex.com/g/bibtex-format/")
                     ("Diacritics" . "https://en.wikibooks.org/wiki/LaTeX/Special_Characters")
                     )
                    )
(spec-handling-add! auto-modes
                    '(bibtex
                      ("\\.bib\\'" . bibtex-mode)
                      ("\\.bst\\'" . bibtex-style-mode)
                      ("\\.bbx\\'" . latex-mode)
                      ("\\.cbx\\'" . latex-mode)
                      )
                    )
;;-- end specs
