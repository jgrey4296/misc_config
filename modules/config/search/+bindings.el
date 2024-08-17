;;; +bindings.el -*- lexical-binding: t; -*-


(map! :leader
      (:prefix "f"
       :desc "Fd File" "l" #'fd-name-dired
       )
      (:prefix ("n" . "notes")
       :desc "Search notes for symbol"        "." #'+default/search-notes-for-symbol-at-point
       :desc "Search notes"                   "s" #'+default/org-notes-search
       :desc "Search org agenda headlines"    "S" #'+default/org-notes-headlines
        )
      )

(map! :map jg-binding-jump-map
      :desc "Open to visible link"         "L" #'link-hint-open-link
      (:prefix ("o" . "Org")
       :desc "Search Notes Headlines" "h" #'+default/org-notes-headlines
       )
      (:prefix "/"
       :desc "Jump to link"                 "L" #'ffap-menu
       :desc "Search current directory"    "d"  #'fd-dired
       :desc "Search Notes"                "n"  #'+default/search-notes-for-symbol-at-point
       :desc "Search Org Dir"              "o"  #'+default/org-notes-search
      )
      )
