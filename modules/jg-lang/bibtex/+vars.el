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
(after! jg-bibtex-vars-go
  (setq-default bibtex-user-optional-fields          nil
                bibtex-completion-bibliography       nil
                bibtex-field-indentation             1
                bibtex-text-indentation              15
                bibtex-align-at-equal-sign           t
                )
  )
;;-- end general bibtex settings

;;-- bibtex fields
(setq org-bibtex-fields '(
                          (:address              . "Usually the address of the publisher or other type of institution.  For major publishing houses, van Leunen recommends omitting the information entirely.  For small publishers, on the other hand, you can help the reader by giving the complete address.")
                          (:annote               . "An annotation.  It is not used by the standard bibliography styles, but may be used by others that produce an annotated bibliography.")
                          (:author               . "The name(s) of the author(s), in the format described in the LaTeX book.  Remember, all names are separated with the and keyword, and not commas.")
                          (:booktitle            . "Title of a book, part of which is being cited.  See the LaTeX book for how to type titles.  For book entries, use the title field instead.")
                          (:chapter              . "A chapter (or section or whatever) number.")
                          (:crossref             . "The database key of the entry being cross referenced.")
                          (:doi                  . "The digital object identifier.")
                          (:edition              . "The edition of a book for example, 'Second'.  This should be an ordinal, and should have the first letter capitalized, as shown here; the standard styles convert to lower case when necessary.")
                          (:editor               . "Name(s) of editor(s), typed as indicated in the LaTeX book.  If there is also an author field, then the editor field gives the editor of the book or collection in which the reference appears.")
                          (:institution          . "The sponsoring institution of a technical report.")
                          (:journal              . "A journal name.")
                          (:key                  . "Used for alphabetizing, cross-referencing, and creating a label when the author information is missing.  This field should not be confused with the key that appears in the \\cite command and at the beginning of the database entry.")
                          (:month                . "The month in which the work was published or, for an unpublished work, in which it was written.  You should use the standard three-letter abbreviation,")
                          (:note                 . "Any additional information that can help the reader.  The first word should be capitalized.")
                          (:number               . "Any additional information that can help the reader.  The first word should be capitalized.")
                          (:organization         . "The organization that sponsors a conference or that publishes a manual.")
                          (:pages                . "One or more page numbers or range of numbers, such as 42-111 or 7,41,73-97 or 43+ (the ‘+’ in this last example indicates pages following that don’t form simple range). BibTEX requires double dashes for page ranges (--).")
                          (:unorthodox           . "How something strange has been published.  The first word should be capitalized.")
                          (:publisher            . "The publisher’s name.")
                          (:school               . "The name of the school where a thesis was written.")
                          (:series               . "The name of a series or set of books.  When citing an entire book, the title field gives its title and an optional series field gives the name of a series or multi-volume set in which the book is published.")
                          (:title                . "The work’s title, typed as explained in the LaTeX book.")
                          (:type                 . "The type of a technical report for example, 'Research Note'.")
                          (:url                  . "Uniform resource locator.")
                          (:volume               . "The volume of a journal or multi-volume book.")
                          (:year                 . "The year of publication or, for an unpublished work, the year it was written.  Generally it should consist of four numerals, such as 1984, although the standard styles can handle any year whose last four nonpunctuation characters are numerals, such as '(about 1984)'")
                          )
      )
(setq jg-bibtex-optional-fields '(("annotation" "Personal Annotation")
                                  ("tags" "Set of tags")
                                  ("isbn" "ISBN of file")
                                  ("doi" "DOI of file")
                                  ("url" "Url of file")
                                  ("file" "The path of the file")
                                  ("translator" "The Translators of the work")
                                  )
      )
(setq jg-bibtex-field-rejections '("date"
                                   "editora"
                                   "editorb"
                                   "editorc"
                                   )
      )
(setq jg-bibtex-search-fields               '("tags" "year")
      jg-bibtex-scholar-search-fields       '("author" "editor" "ALTauthor" "Alteditor" "year" "doi" "isbn")
      jg-bibtex-scholar-search-fields-exact '("title")
      )
(after! jg-bibtex-vars-go
  (setq bibtex-completion-additional-search-fields jg-bibtex-search-fields
        bibtex-completion-pdf-field                "file")
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
                              ;; generate key
                              +jg-bibtex-orcb-key-hook
                              +jg-bibtex-insert-volume-to-key
                              ;; Final alignment and indent
                              +jg-bibtex-align-hook
                              +jg-bibtex-indent-hook
                              )
      )
;;-- end cleaning hooks

;;-- urls
(setq jg-bibtex-scholar-search-string "https://scholar.google.com/scholar?hl=en&q=%s"
      jg-bibtex-dblp-search-string    "https://dblp1.uni-trier.de/search?q=%s"
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
(setq jg-bibtex-open-pdf-cmd  "open -nF "
      jg-bibtex-open-epub-cmd "open -a ebook-viewer "
      jg-bibtex-curl-cmd      "curl"
      jg-bibtex-curl-args     "-sLI"
      bibtex-completion-pdf-open-function #'(lambda (x) (org-open-link-from-string (format "[[file:%s]]" x)))
 )
;;-- end commands

;;-- hl line
(after! hl-line
  (add-to-list 'global-hl-line-modes 'bibtex-mode)
  )
;;-- end hl line
