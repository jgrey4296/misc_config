;;; +entries.el -*- lexical-binding: t; -*-


;; (entry-type doc required crossref optional)
;; req/cross/opt fields are lists of:
;; (field comment init alt)

;; keep these synced with jg_custom.bst
(defvar bibtex-jg-entry-alist nil)

;;-- standard
(pushnew! bibtex-jg-entry-alist
          '("article" "Article in Journal"
            ( ("author") ("title") ("tags") ("doi") )
            ( ("year") ("journal") ("volume") ("number") )
            ( ("pages") ("subtitle") ) )
          '("book" "Single-Volume Book"
            (("author" nil nil 1) ("editor" nil nil -1) ("title")  ("year" ) ("publisher") ("isbn") ("edition_year"))
            ()
            ( ("series")  ("tags") ("subtitle") ) )
          '("collection" "Single-Volume Collection"
            ( ("title")  ("tags") ("author" nil nil 1) ("editor" nil nil -1)  ("year")  )
            ()
            ( ("tags") ("subtitle") ) )
          '("proceedings" "Conference Proceedings"
            ( ("title") ("editor") ("year" ) ("country") ("url" nil nil 1) ("isbn" nil nil -1) )
            ()
            ( ("tags") ("doi") ("subtitle") ) )
          '("booklet" "Booklet (Bound, but no Publisher)"
            ( ("author" nil nil 1) ("editor" nil nil -1) ("title") ("tags")  ("year") )
            ()
            ( ("subtitle")) )
          )
;;-- end standard

;;-- misc
(pushnew! bibtex-jg-entry-alist
          '("misc" "Miscellaneous"
            ( ("author") ("title") ("year" ) ("tags") ("url") )
            ()
            () )
          '("unpublished" "Unpublished"
            ( ("author") ("title")  ("year" ) ("tags") ("url") )
            ()
            () )
          '("musicScore" ""
            ( ("author") ("tags") ("year") ("idenfitied") ("title") )
            ()
            ( ("arrangement") ("section") ) )
          '("review" ""
            ( ("author") ("tags") ("year") ("book_author") ("book_publisher") ("title") )
            ()
            ( ("journal") ("url") ("volume") ("number") ("pages") ("review_crossref") ("isbn") ) )
          )
;;-- end misc

;;-- digital
(pushnew! bibtex-jg-entry-alist
          '("online" "Online Resource"
            ( ("author" ) ("title") ("year" ) ("url" ) ("tags") )
            ()
            () )
          '("software" "Computer Software"
            ( ("author" ) ("title")  ("year" ) ("platform") ("tags") ("number") )
            ()
            () )
          '("game" ""
            ( ("author") ("tags") ("year") ("platform") ("title") ("url") )
            ()
            ( ("publisher") ("series") ("edition") ("number") )
            )
          '("blog" ""
            ( ("author") ("tags") ("year") ("url") ("title") )
            ()
            () )
          '("tweet" ""
            ( ("author") ("tags") ("year") ("url") )
            ()
            ( ("title") ) )
          '("thread" ""
            ( ("author") ("tags") ("year") ("url") ("title") )
            ()
            () )
          '("video" ""
            (("author") ("tags") ("year") ("url") ("channel") ("series"))
            ()
            ()
            )
          )
;;-- end digital

;;-- thesis
(pushnew! bibtex-jg-entry-alist
          '("thesis" "PhD or Master's Thesis"
            ( ("author") ("title")  ("institution" ) ("year") ("type") )
            ()
            ( ("doi") ("subtitle")) )
          ;; '("phdthesis" "Phd Thesis"
          ;;   ( ("author") ("title")  ("institution" ) ("year") )
          ;;   ()
          ;;   ( ("doi") ("subtitle")) )
          ;; '("mastersthesis" "Master's Thesis"
          ;;   ( ("author") ("title")  ("institution" ) ("year") )
          ;;   ()
          ;;   ( ("doi")  ) )
          )
;;-- end thesis

;;-- part of something
(pushnew! bibtex-jg-entry-alist
          '("inbook" "Chapter or Pages in a Book"
            ( ("title") ("tags") ("doi") )
            ( ("author")  ("year")  ("booktitle") ("isbn") ("publisher") )
            ( ("subtitle") ))
          '("incollection" "Article in a Collection"
            ( ("author") ("title")  ("tags") ("doi") )
            ( ("booktitle")  ("year")  ("isbn") ("publisher") )
            ( ("subtitle") ) )
          '("inproceedings" "Article in Conference Proceedings"
            ( ("author") ("title") ("year" ) ("tags") ("doi") )
            ( ("booktitle") )
            ( ("subtitle")) )
          )
;;-- end part of something

;;-- technical
(pushnew! bibtex-jg-entry-alist
          '("dataset" "Data Set"
            ( ("author" ) ("editor" ) ("title") ("tags")  ("year") ("institution") )
            ()
            () )
          '("manual" "Technical Manual"
            ( ("author" nil nil 1) ("editor" nil nil -1) ("title") ("year") ("tags") ("url") )
            ()
            () )
          '("report" "Technical or Research Report"
            ( ("author") ("title")  ("institution" ) ("year" ) ("tags") ("url") )
            ()
            ( ("country")) )
          '("standard" "A Technical Standard. eg: by w3, IEEE..."
            ( ("title") ("tags") ("identifier") ("institution") ("year") )
            ()
            ( ("section") ) )
          '("techreport" ""
            ( ("author") ("title")  ("year" ) ("tags") ("url") ("identifier") ("institution") ("url") )
            ()
            ( ("doi") ) )
          )
;;-- end technical

;;-- legal
(pushnew! bibtex-jg-entry-alist
          '("case" "A Legal Case"
            ( ("plaintiff") ("defendant") ("year") ("identifier") ("country") ("tags") )
            ()
            ( ("short_parties") ) )
          '("judicial" "A Judicial Opinion"
            ( ("author" ) ("year") ("tags") ("institution") )
            ( ("identifier") ("country") ("plaintiff") ("defendant") )
            ( ("short_parties") ("dissent") ("concur") ) )
          '("law" "A Proposed or Enacted Law"
            ( ("year") ("tags") ("institution") ("status") ("identifier") ("url") ("country"))
            ()
            ( ("author") ("section") ) )
          )
;;-- end legal
