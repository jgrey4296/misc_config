;;; +bindings.el -*- lexical-binding: t; -*-


(map! :map jg-help-map
      :after jg-help-bindings
      :desc "Load Bookmarks"              "r b" #'+jg-nav-bookmarks-load

      :prefix ("e B" . "Bookmarks")
      :desc "Add bookmark"                "a" #'bookmark-set
      :desc "Delete bookmark"             "k" #'bookmark-delete
      :desc "Rename bookmark"             "r" #'bookmark-rename
      :desc "Save Bookmarks"              "s" #'bookmark-save
      :desc "Load Bookmarks"              "l" #'+jg-nav-bookmarks-load
      )

(map! :map jg-binding-jump-map
      :desc "Jump to Window"  "w" #'switch-window
      )

(map! :map jg-binding-helm-map
      :desc "Firefox Helm"    "f" #'+jg-nav-helm-bookmarks
      )

(map! :leader
      (:prefix "w"
       :desc "3 Column Centered" "3" #'+jg-windows-3-col-centered
       :desc "Reserve" "r"           #'+jg-windows-toggle-dedicated
       )
      (:prefix "o s"
       :desc "Pop up scratch buffer"        "s"   #'+jg-window-nav--system-scratch
       :desc "open project scratch buffer"  "p"   #'+jg-window-nav--project-scratch
       )
)
