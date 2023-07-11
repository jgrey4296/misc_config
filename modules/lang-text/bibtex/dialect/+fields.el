;;; +fields.el -*- lexical-binding: t; -*-

;; Keep these synced with jg_custom.bst

(defvar bibtex-jg-field-alist nil)

;;-- basic
(pushnew! bibtex-jg-field-alist
          '(:address)
          '(:annotation    . "Personal annotation")
          '(:annote)
          '(:archive)
          '(:author        . "The name(s) of the author(s), in the format described in the LaTeX book.  Remember, all names are separated with the and keyword, and not commas.")
          '(:booktitle     . "Title of a book, part of which is being cited.  See the LaTeX book for how to type titles.  For book entries, use the title field instead.")
          '(:chapter       . "A chapter (or section or whatever) number.")
          '(:crossref      . "The database key of the entry being cross referenced.")
          '(:edition       . "The edition of a book for example, 'Second'.  This should be an ordinal, and should have the first letter capitalized, as shown here; the standard styles convert to lower case when necessary.")
          '(:editor        . "Name(s) of editor(s), typed as indicated in the LaTeX book.  If there is also an author field, then the editor field gives the editor of the book or collection in which the reference appears.")
          '(:file)
          '(:institution   . "The sponsoring institution of a technical report.")
          '(:journal       . "A journal name.")
          '(:key           . "Used for alphabetizing, cross-referencing, and creating a label when the author information is missing.  This field should not be confused with the key that appears in the \\cite command and at the beginning of the database entry.")
          '(:note          . "Any additional information that can help the reader.  The first word should be capitalized.")
          '(:number        . "Any additional information that can help the reader.  The first word should be capitalized.")
          '(:pages         . "One or more page numbers or range of numbers, such as 42-111 or 7,41,73-97 or 43+ (the ‘+’ in this last example indicates pages following that don’t form simple range). BibTEX requires double dashes for page ranges (--).")
          '(:publisher     . "The publisher’s name.")
          '(:section)
          '(:series        . "The name of a series or set of books.  When citing an entire book, the title field gives its title and an optional series field gives the name of a series or multi-volume set in which the book is published.")
          '(:tags)
          '(:title         . "The work’s title, typed as explained in the LaTeX book.")
          '(:translator)
          '(:collaboration)
          '(:type          . "The type of a technical report for example, 'Research Note'.")
          '(:unorthodox    . "How something strange has been published.  The first word should be capitalized.")
          '(:volume        . "The volume of a journal or multi-volume book.")
          '(:year          . "The year of publication or, for an unpublished work, the year it was written.  Generally it should consist of four numerals, such as 1984, although the standard styles can handle any year whose last four nonpunctuation characters are numerals, such as '(about 1984)'")
          )
;;-- end basic

;;-- online lookup
(pushnew! bibtex-jg-field-alist
          '(:doi           . "The digital object identifier.")
          '(:eid)
          '(:eprint)
          '(:isbn)
          '(:issn)
          '(:url           . "Uniform resource locator.")
          )
;;-- end online lookup

;;-- music
(pushnew! bibtex-jg-field-alist
          '(:arrangement   . "The arrangement of the score")
)
;;-- end music

;;-- software
(pushnew! bibtex-jg-field-alist
          '(:platform      . "The system the software runs on")
)
;;-- end software

;;-- law
(pushnew! bibtex-jg-field-alist
          '(:concur)
          '(:counsel)
          '(:country)
          '(:defendant)
          '(:dissent)
          '(:identifier    . "The case number, bill number, standard number etc")
          '(:plaintiff)
          '(:short_parties . "The short name of the plaintiffs and defendants")
          '(:status        . "The Status of the legislation/standard/opinion. Current, repealed, overruled etc")
          )
;;-- end law

;;-- reviews
(pushnew! bibtex-jg-field-alist
;;           '(:bookpublisher)
          '(:review_crossref)
;;           '(:bookauthor)
          )
;;-- end reviews
