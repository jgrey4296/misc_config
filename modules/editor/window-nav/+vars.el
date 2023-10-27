;;; +vars.el -*- lexical-binding: t; -*-

(defvar jg-nav-loc-bookmarks  (expand-file-name "~/github/bibliography/bookmarks/total.bookmarks"))

(after! ivy
  (ivy-add-actions 'counsel-evil-marks
                   '(("m" +jg-navigation-marker-delete-action "Delete Marks")))
  )

(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1

      split-width-threshold 160
      split-height-threshold nil
      use-short-answers t
      )
