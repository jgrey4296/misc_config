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

;; switch window
(setq switch-window-shortcut-appearance 'asciiart
      switch-window-shortcut-style 'qwerty
      switch-window-minibuffer-shortcut ?z
      )


;; Ace Window
(setq aw-char-position 'left
      aw--lead-overlay-fn #'aw--lead-overlay
      )

(spec-handling-add! popup :form 'override
                    '(window-nav
                      ("\*scratch::system\\*"   :side left   :ttl nil :width  50  :quit t :select t :priority 150)
                      ("\*scratch::.*?\\*"      :side right  :ttl nil :width  50  :quit t :select t :priority 150)
                      )
                    )
