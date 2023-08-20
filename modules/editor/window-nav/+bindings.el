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
      :prefix "a"
      :desc "Firefox Helm"                "f" #'+jg-nav-helm-bookmarks
      )
