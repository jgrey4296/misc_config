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

(after! bibtex
  (pushnew! bibtex-dialect-list 'jg)
  )
;;-- end personal settings

;;-- general bibtex settings
(after! jg-bibtex-vars-go
  (setq-default bibtex-user-optional-fields          nil
                bibtex-completion-bibliography       nil
                bibtex-field-indentation             1
                bibtex-text-indentation              15
                bibtex-align-at-equal-sign           t
                org-bibtex-export-arbitrary-fields   t
                )
  )
;;-- end general bibtex settings

;;-- bibtex entry types
;; (entry-type doc required crossref optional)
;; req/cross/opt fields are lists of:
;; (field comment init alt)

;; keep these synced with jg_custom.bst
(setq bibtex-jg-entry-alist
      '(
        ("Article" "Article in Journal"
         ( ("author") ("title") ("tags") ("doi") )
         ( ("year") ("journal") ("volume") ("number") )
         ( ("pages") ) )
        ("Book" "Single-Volume Book"
         (("author" nil nil 1) ("editor" nil nil -1) ("title")  ("year" ) ("publisher") ("isbn") )
         ()
         ( ("series")  ("tags")  ) )
        ("InBook" "Chapter or Pages in a Book"
         ( ("title") ("tags") ("doi") )
         ( ("author")  ("year")  ("booktitle") ("isbn") ("publisher") )
         ())
        ("Booklet" "Booklet (Bound, but no Publisher)"
         ( ("author" nil nil 1) ("editor" nil nil -1) ("title") ("tags")  ("year") )
         ()
         () )
        ("Collection" "Single-Volume Collection"
         ( ("title")  ("tags") ("author" nil nil 1) ("editor" nil nil -1)  ("year")  )
         ()
         ( ("tags") ) )
        ("InCollection" "Article in a Collection"
         ( ("author") ("title")  ("tags") ("doi") )
         ( ("booktitle")  ("year")  ("isbn") ("publisher") )
         () )
        ("Dataset" "Data Set"
         ( ("author" ) ("editor" ) ("title") ("tags")  ("year") ("institution") )
         ()
         () )
        ("Manual" "Technical Manual"
         ( ("author" nil nil 1) ("editor" nil nil -1) ("title") ("year") ("tags") ("url") )
         ()
         () )
        ("Misc" "Miscellaneous"
         ( ("author") ("title") ("year" ) ("tags") ("url") )
         ()
         () )
        ("Online" "Online Resource"
         ( ("author" ) ("editor" ) ("title") ("year" ) ("doi" ) ("url" ) ("tags") )
         ()
         () )
        ("Proceedings" "Conference Proceedings"
         ( ("title") ("editor") ("year" ) ("country") ("url" nil nil 1) ("isbn" nil nil -1) )
         ()
         ( ("tags") ("doi") ) )
        ("InProceedings" "Article in Conference Proceedings"
         ( ("author") ("title") ("year" ) ("tags") ("doi") )
         ( ("booktitle") )
         () )
        ("Report" "Technical or Research Report"
         ( ("author") ("title")  ("institution" ) ("year" ) ("tags") )
         ()
         () )
        ("Software" "Computer Software"
         ( ("author" ) ("title")  ("year" ) ("platform") ("tags") )
         ()
         () )
        ("Thesis" "PhD or Master's Thesis"
         ( ("author") ("title")  ("institution" ) ("year") ("type") )
         ()
         ( ("doi")) )
        ("phdthesis" "Phd Thesis"
        ( ("author") ("title")  ("institution" ) ("year") )
        ()
        ( ("doi") ) )
        ("mastersthesis" "Master's Thesis"
        ( ("author") ("title")  ("institution" ) ("year") )
        ()
        ( ("doi") ) )
        ("Unpublished" "Unpublished"
         ( ("author") ("title")  ("year" ) ("tags") ("url") )
         ()
         () )
        ("TechReport" ""
         ( ("author") ("title")  ("year" ) ("tags") ("url") ("identifier") ("institution") )
         ()
         ( ("doi") ) )
        ("Case" "A Legal Case"
         ( ("plaintiff") ("defendant") ("year") ("identifier") ("country") ("tags") )
         ()
         ( ("short_parties") ) )
        ("Judicial" "A Judicial Opinion"
         ( ("author" ) ("year") ("tags") ("institution") )
         ( ("identifier") ("country") ("plaintiff") ("defendant") )
         ( ("short_parties") ("dissent") ("concur") ) )
        ("Law" "A Proposed or Enacted Law"
         ( ("year") ("tags") ("institution") ("status") ("identifier") )
         ()
         ( ("author") ("section") ) )
        ("Standard" "A Technical Standard. eg: by w3, IEEE..."
         ( ("title") ("tags") ("identifier") ("institution") ("year") )
         ()
         ( ("section") ) )
        ("Game" ""
         ( ("author") ("tags") ("year") ("platform") ("title") ("url") )
         ()
         ( ("publisher") ("series") ("edition") ("number") )
         )
        ("Blog" ""
         ( ("author") ("tags") ("year") ("url") ("title") )
         ()
         () )
        ("Tweet" ""
         ( ("author") ("tags") ("year") ("url") )
         ()
         ( ("title") ) )
        ("Thread" ""
         ( ("author") ("tags") ("year") ("url") ("title") )
         ()
         () )
        ("MusicScore" ""
         ( ("author") ("tags") ("year") ("idenfitied") ("title") )
         ()
         ( ("arrangement") ("section") ) )
        )
      )

;;-- end bibtex entry types

;;-- bibtex fields
;; Keep these synced with jg_custom.bst
(setq bibtex-jg-field-alist '(
                              (:counsel)
                              (:dissent)
                              (:concur)
                              (:Custom)
                              (:address       . "Usually the address of the publisher or other type of institution.  For major publishing houses, van Leunen recommends omitting the information entirely.  For small publishers, on the other hand, you can help the reader by giving the complete address.")
                              (:annote        . "An annotation.  It is not used by the standard bibliography styles, but may be used by others that produce an annotated bibliography.")
                              (:archive)
                              (:author        . "The name(s) of the author(s), in the format described in the LaTeX book.  Remember, all names are separated with the and keyword, and not commas.")
                              (:booktitle     . "Title of a book, part of which is being cited.  See the LaTeX book for how to type titles.  For book entries, use the title field instead.")
                              (:chapter       . "A chapter (or section or whatever) number.")
                              (:collaboration)
                              (:country)
                              (:crossref      . "The database key of the entry being cross referenced.")
                              (:doi           . "The digital object identifier.")
                              (:edition       . "The edition of a book for example, 'Second'.  This should be an ordinal, and should have the first letter capitalized, as shown here; the standard styles convert to lower case when necessary.")
                              (:editor        . "Name(s) of editor(s), typed as indicated in the LaTeX book.  If there is also an author field, then the editor field gives the editor of the book or collection in which the reference appears.")
                              (:eid)
                              (:eprint)
                              (:institution   . "The sponsoring institution of a technical report.")
                              (:isbn)
                              (:issn)
                              (:journal       . "A journal name.")
                              (:key           . "Used for alphabetizing, cross-referencing, and creating a label when the author information is missing.  This field should not be confused with the key that appears in the \\cite command and at the beginning of the database entry.")
                              (:month         . "The month in which the work was published or, for an unpublished work, in which it was written.  You should use the standard three-letter abbreviation,")
                              (:note          . "Any additional information that can help the reader.  The first word should be capitalized.")
                              (:number        . "Any additional information that can help the reader.  The first word should be capitalized.")
                              (:pages         . "One or more page numbers or range of numbers, such as 42-111 or 7,41,73-97 or 43+ (the ‘+’ in this last example indicates pages following that don’t form simple range). BibTEX requires double dashes for page ranges (--).")
                              (:platform)
                              (:publisher     . "The publisher’s name.")
                              (:series        . "The name of a series or set of books.  When citing an entire book, the title field gives its title and an optional series field gives the name of a series or multi-volume set in which the book is published.")
                              (:tags)
                              (:title         . "The work’s title, typed as explained in the LaTeX book.")
                              (:type          . "The type of a technical report for example, 'Research Note'.")
                              (:unorthodox    . "How something strange has been published.  The first word should be capitalized.")
                              (:url           . "Uniform resource locator.")
                              (:volume        . "The volume of a journal or multi-volume book.")
                              (:year          . "The year of publication or, for an unpublished work, the year it was written.  Generally it should consist of four numerals, such as 1984, although the standard styles can handle any year whose last four nonpunctuation characters are numerals, such as '(about 1984)'")
                              (:annotation    . "Personal annotation")
                              (:platform      . "The system the software runs on")
                              (:status        . "The Status of the legislation/standard/opinion. Current, repealed, overruled etc")
                              (:plaintiff)
                              (:defendant)
                              (:short_parties . "The short name of the plaintiffs and defendants")
                              (:identifier    . "The case number, bill number, standard number etc")
                              (:arrangement   . "The arrangement of the score")
                              (:section)
                              (:file)
                              (:translator)
                              )
      )
(setq jg-bibtex-search-fields               '("tags" "year" "publisher")
      jg-bibtex-scholar-search-fields       '("author" "editor" "ALTauthor" "Alteditor" "year" "doi" "isbn")
      jg-bibtex-scholar-search-fields-exact '("title")
      )
(after! jg-bibtex-vars-go
  (setq bibtex-completion-additional-search-fields jg-bibtex-search-fields
        bibtex-completion-pdf-field                "file")
  )
(after! bibtex
  (setq bibtex-dialect 'jg)
  )
;;-- end bibtex fields

;;-- cleaning hooks
(setq jg-bibtex-clean-hooks '(+jg-bibtex-insert-stub-key ;; Initial key
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
;;-- end cleaning hooks

;;-- urls
(setq jg-bibtex-reference-url         "https://www.bibtex.com/g/bibtex-format/"
      jg-bibtex-doi-url                "https://doi.org/%s"
      )
;;-- end urls

;;-- strings
(setq jg-bibtex-completion-display-formats
      '(
        (t . ("${author:20} || ${title:*} || ${year:4}" 40))
        )
      )
(setq jg-bibtex-pdf-loc-regexp               "file[[:digit:]]*\s*=\s*{\\(.+\\)/\\(.+pdflibrary\\)?"
      jg-bibtex-pdf-replace-match-string     "~/"
      jg-bibtex-pdf-replace-library-string   "pdflibrary"
      jg-bibtex-remove-field-newlines-regexp "^="
      jg-bibtex-tweet-pattern                "(%s): %s\nby %s\nTags: %s\nRef: %s\n"
 )
;;-- end strings

;;-- locations
(setq-default jg-bibtex-loc-bibtex          "~/github/writing/resources/bibliography/"
              jg-bibtex-todo-loc            "~/github/writing/resources/todo.bib"
              jg-bibtex-loc-completions     "~/github/writing/resources/completions/"
              jg-bibtex-loc-export-bib-file "~/github/emacs_files/modules/jg-lang/bibtex/export_template.tex"
              jg-bibtex-loc-temp-dir        "~/.tex/"
              jg-bibtex-pdf-loc             "~/pdflibrary"
 )
;;-- end locations

;;-- commands
(setq jg-bibtex-curl-cmd      "curl"
      jg-bibtex-curl-args     "-sLI"
      ;; bibtex-completion-pdf-open-function #'(lambda (x) (org-open-link-from-string (format "[[file:%s]]" x)))
      bibtex-completion-pdf-open-function 'browse-url
 )
;;-- end commands

;;-- hl line
(after! hl-line
  (add-to-list 'global-hl-line-modes 'bibtex-mode)
  )
;;-- end hl line
