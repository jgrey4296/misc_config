;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map jg-binding-jump-map
      :desc "Paren State" "p"  #'evil-paren-state
      )

(map! :map evil-paren-state-map
      "a" #'evil-toggle-fold
      "z" 'jg-binding-vision-map
      )

(map! :leader
      :prefix ("B" . "Bookmarks")
      :desc "Set bookmark"                "m" #'bookmark-set
      :desc "Delete bookmark"             "M" #'bookmark-delete
      :desc "Rename bookmark"             "r" #'bookmark-rename
      :desc "Save Bookmarks"              "s" #'bookmark-save
      :desc "Load Bookmarks"              "l" #'+jg-nav-bookmarks-load
      :desc "Firefox Helm"                "f" #'+jg-nav-helm-bookmarks
      )
