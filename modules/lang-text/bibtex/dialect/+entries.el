;;; +entries.el -*- lexical-binding: t; -*-


;; (entry-type doc required crossref optional)
;; req/cross/opt fields are lists of:
;; (field comment init alt)

;; keep these synced with jg_custom.bst
(defvar bibtex-jg-entry-alist nil)

;;-- standard
(pushnew! bibtex-jg-entry-alist
          '("Article" "Article in Journal"
            ( ("author") ("title") ("tags") ("doi") )
            ( ("year") ("journal") ("volume") ("number") )
            ( ("pages") ) )
          '("Book" "Single-Volume Book"
            (("author" nil nil 1) ("editor" nil nil -1) ("title")  ("year" ) ("publisher") ("isbn") )
            ()
            ( ("series")  ("tags")  ) )
          '("Collection" "Single-Volume Collection"
            ( ("title")  ("tags") ("author" nil nil 1) ("editor" nil nil -1)  ("year")  )
            ()
            ( ("tags") ) )
          '("Proceedings" "Conference Proceedings"
            ( ("title") ("editor") ("year" ) ("country") ("url" nil nil 1) ("isbn" nil nil -1) )
            ()
            ( ("tags") ("doi") ) )
          '("Booklet" "Booklet (Bound, but no Publisher)"
            ( ("author" nil nil 1) ("editor" nil nil -1) ("title") ("tags")  ("year") )
            ()
            () )
          )
;;-- end standard

;;-- misc
(pushnew! bibtex-jg-entry-alist
          '("Misc" "Miscellaneous"
            ( ("author") ("title") ("year" ) ("tags") ("url") )
            ()
            () )
          '("Unpublished" "Unpublished"
            ( ("author") ("title")  ("year" ) ("tags") ("url") )
            ()
            () )
          '("MusicScore" ""
            ( ("author") ("tags") ("year") ("idenfitied") ("title") )
            ()
            ( ("arrangement") ("section") ) )
          '("Review" ""
            ( ("author") ("tags") ("year") ("bookauthor") ("bookpublisher") ("title") )
            ()
            ( ("journal") ("url") ("volume") ("number") ("pages") ("review_crossref") ("isbn") ) )
          )
;;-- end misc

;;-- digital
(pushnew! bibtex-jg-entry-alist
          '("Online" "Online Resource"
            ( ("author" ) ("title") ("year" ) ("url" ) ("tags") )
            ()
            () )
          '("Software" "Computer Software"
            ( ("author" ) ("title")  ("year" ) ("platform") ("tags") ("number") )
            ()
            () )
          '("Game" ""
            ( ("author") ("tags") ("year") ("platform") ("title") ("url") )
            ()
            ( ("publisher") ("series") ("edition") ("number") )
            )
          '("Blog" ""
            ( ("author") ("tags") ("year") ("url") ("title") )
            ()
            () )
          '("Tweet" ""
            ( ("author") ("tags") ("year") ("url") )
            ()
            ( ("title") ) )
          '("Thread" ""
            ( ("author") ("tags") ("year") ("url") ("title") )
            ()
            () )
          '("Video" ""
            (("author") ("tags") ("year") ("url") ("channel") ("series"))
            ()
            ()
            )
          )
;;-- end digital

;;-- thesis
(pushnew! bibtex-jg-entry-alist
          '("Thesis" "PhD or Master's Thesis"
            ( ("author") ("title")  ("institution" ) ("year") ("type") )
            ()
            ( ("doi")) )
          '("phdthesis" "Phd Thesis"
            ( ("author") ("title")  ("institution" ) ("year") )
            ()
            ( ("doi") ) )
          '("mastersthesis" "Master's Thesis"
            ( ("author") ("title")  ("institution" ) ("year") )
            ()
            ( ("doi") ) )
          )
;;-- end thesis

;;-- part of something
(pushnew! bibtex-jg-entry-alist
          '("InBook" "Chapter or Pages in a Book"
            ( ("title") ("tags") ("doi") )
            ( ("author")  ("year")  ("booktitle") ("isbn") ("publisher") )
            ())
          '("InCollection" "Article in a Collection"
            ( ("author") ("title")  ("tags") ("doi") )
            ( ("booktitle")  ("year")  ("isbn") ("publisher") )
            () )
          '("InProceedings" "Article in Conference Proceedings"
            ( ("author") ("title") ("year" ) ("tags") ("doi") )
            ( ("booktitle") )
            () )
          )
;;-- end part of something

;;-- technical
(pushnew! bibtex-jg-entry-alist
          '("Dataset" "Data Set"
            ( ("author" ) ("editor" ) ("title") ("tags")  ("year") ("institution") )
            ()
            () )
          '("Manual" "Technical Manual"
            ( ("author" nil nil 1) ("editor" nil nil -1) ("title") ("year") ("tags") ("url") )
            ()
            () )
          '("Report" "Technical or Research Report"
            ( ("author") ("title")  ("institution" ) ("year" ) ("tags") )
            ()
            () )
          '("Standard" "A Technical Standard. eg: by w3, IEEE..."
            ( ("title") ("tags") ("identifier") ("institution") ("year") )
            ()
            ( ("section") ) )
          '("TechReport" ""
            ( ("author") ("title")  ("year" ) ("tags") ("url") ("identifier") ("institution") )
            ()
            ( ("doi") ) )
          )
;;-- end technical

;;-- legal
(pushnew! bibtex-jg-entry-alist
          '("Case" "A Legal Case"
            ( ("plaintiff") ("defendant") ("year") ("identifier") ("country") ("tags") )
            ()
            ( ("short_parties") ) )
          '("Judicial" "A Judicial Opinion"
            ( ("author" ) ("year") ("tags") ("institution") )
            ( ("identifier") ("country") ("plaintiff") ("defendant") )
            ( ("short_parties") ("dissent") ("concur") ) )
          '("Law" "A Proposed or Enacted Law"
            ( ("year") ("tags") ("institution") ("status") ("identifier") )
            ()
            ( ("author") ("section") ) )
          )
;;-- end legal
