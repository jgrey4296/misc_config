;;; +bindings.el -*- lexical-binding: t; -*-


(map! :leader
      (:prefix "f"
       :desc "Fd File" "l" #'fd-name-dired
       )
      )

(map! :leader
      :prefix ("s" . "search")
      :desc "Jump to link"                 "L" #'ffap-menu
      :desc "Open visible link"            "l" #'link-hint-open-link
      :desc "Search project"               "p" #'+default/search-project
      :desc "Search project for symbol"    "." #'+default/search-project-for-symbol-at-point
      :desc "Search current directory"     "d" #'+default/search-cwd
      :desc "Fd File"                      "f" #'fd-dired
      )

(map! :map jg-binding-jump-map
      :desc "Open to visible link"         "L" #'link-hint-open-link
      )
