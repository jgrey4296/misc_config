;;; domain-specific/bibtex/+vars.el -*- lexical-binding: t; -*-

(defvar jg-bibtex-mode-map (make-sparse-keymap))

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

;;-- bibtex fields
(setq bibtex-completion-additional-search-fields jg-bibtex-search-fields
      bibtex-completion-pdf-field                "file")
;;-- end bibtex fields

;;-- hl line
(after! hl-line
  (add-to-list 'global-hl-line-modes 'bibtex-mode)
  )
;;-- end hl line

;;-- librarian
(setq librarian-biblio-pdf-loc (pcase system-type
                                     ('darwin (expand-file-name "~/pdf_library"))
                                     ('gnu/linux "/media/john/data/library/pdfs"))
      librarian-biblio-library-loc   (expand-file-name "~/github/bibliography/main/")
      librarian-biblio-unsourced-loc (expand-file-name "~/github/bibligraphy/in_progress/to_source.bib")

      )

;;-- end librarian

;;-- specs
(speckler-add! lookup-url
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
(speckler-add! whitespace-cleanup
                    '(bibtex-mode
                     +jg-bibtex-cleanup-ensure-newline-before-def
                     delete-trailing-whitespace
                     +jg-text-cleanup-whitespace
                     )
                    )
(speckler-add! auto-modes
                    '(bibtex
                      ("\\.bib\\'" . bibtex-mode)
                      ("\\.bst\\'" . bibtex-style-mode)
                      ("\\.bbx\\'" . latex-mode)
                      ("\\.cbx\\'" . latex-mode)
                      )
                    )
(speckler-add! popup
                    '(bibtex
                     ("^\\*DOI Metadata\\*\\'" :side left :ttl 5 :width 0.3 :quit t :select nil :priority 50)
                     ("^\\*Metadata\\*\\'"     :side left :ttl 5 :width 0.3 :quit t :select nil :priority 50)
                     )
                    )

(speckler-add! compile-commands
                    '(bibtex +jg-bibtex-get-commands)
                    )
;;-- end specs
