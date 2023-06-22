;;; +vars.el -*- lexical-binding: t; -*-

(defvar jg-nav-loc-bookmarks  (expand-file-name "~/github/jgrey4296.github.io/resources/bookmarks/total.bookmarks"))

(after! ivy
  (ivy-add-actions 'counsel-evil-marks
                   '(("m" +jg-navigation-marker-delete-action "Delete Marks")))
  )
