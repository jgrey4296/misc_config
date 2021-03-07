;; lang/org/+bindings_2.el -*- lexical-binding: t; -*-
;; Misc
(map! :after org
      :map org-mode-map
      ;; textmate-esque newline insertion
      "S-RET"      #'+org/shift-return
      "C-RET"      #'+org/insert-item-below
      "C-S-RET"    #'+org/insert-item-above
      "C-M-RET"    #'org-insert-subheading
      [C-return]   #'+org/insert-item-below
      [C-S-return] #'+org/insert-item-above
      [C-M-return] #'org-insert-subheading
      (:when IS-MAC
       [s-return]   #'+org/insert-item-below
       [s-S-return] #'+org/insert-item-above
       [s-M-return] #'org-insert-subheading)
      ;; Org-aware C-a/C-e
      [remap doom/backward-to-bol-or-indent]          #'org-beginning-of-line
      [remap doom/forward-to-last-non-comment-or-eol] #'org-end-of-line

      )
;;; Misc 2
(map! :after org
      :map org-mode-map
      :localleader
      "#" #'org-update-statistics-cookies
      "'" #'org-edit-special
      "*" #'org-ctrl-c-star
      "," #'org-switchb
      )

;;; No Prefix
(map! :after org
      :map org-mode-map
      :localleader
      "A" #'org-archive-subtree
      "e" #'org-export-dispatch
      "n" #'org-store-link
      "o" #'org-set-property
      "p" #'org-priority
      "q" #'org-set-tags-command
      "T" #'org-todo-list
      )
;;; <A>
(map! :after org
      :map org-mode-map
      :localleader
      :prefix ("a" . "attachments")
      "a" #'org-attach
      "d" #'org-attach-delete-one
      "D" #'org-attach-delete-all
      "f" #'+org/find-file-in-attachments
      "l" #'+org/attach-file-and-insert-link
      "n" #'org-attach-new
      "o" #'org-attach-open
      "O" #'org-attach-open-in-emacs
      "r" #'org-attach-reveal
      "R" #'org-attach-reveal-in-emacs
      "u" #'org-attach-url
      "s" #'org-attach-set-directory
      "S" #'org-attach-sync
      (:when (featurep! +dragndrop)
       "c" #'org-download-screenshot
       "p" #'org-download-clipboard
       "P" #'org-download-yank)
      )

;;; <B>
(map! :after org
      :map org-mode-map
      :localleader
      :prefix ("b" . "tables")
      "-" #'org-table-insert-hline
      "a" #'org-table-align
      "b" #'org-table-blank-field
      "c" #'org-table-create-or-convert-from-region
      "e" #'org-table-edit-field
      "f" #'org-table-edit-formulas
      "h" #'org-table-field-info
      "s" #'org-table-sort-lines
      "r" #'org-table-recalculate
      "R" #'org-table-recalculate-buffer-tables
      (:prefix ("d" . "delete")
       "c" #'org-table-delete-column
       "r" #'org-table-kill-row)
      (:prefix ("i" . "insert")
       "c" #'org-table-insert-column
       "h" #'org-table-insert-hline
       "r" #'org-table-insert-row
       "H" #'org-table-hline-and-move)

      (:when (featurep! +gnuplot)
       "p" #'org-plot/gnuplot)
      )
;;; <C>
(map! :after org
      :map org-mode-map
      :localleader
      :prefix ("c" . "clock")
      "c" #'org-clock-cancel
      "d" #'org-clock-mark-default-task
      "e" #'org-clock-modify-effort-estimate
      "E" #'org-set-effort
      "g" #'org-clock-goto
      "G" (cmd! (org-clock-goto 'select))
      "l" #'+org/toggle-last-clock
      "i" #'org-clock-in
      "I" #'org-clock-in-last
      "o" #'org-clock-out
      "r" #'org-resolve-clocks
      "R" #'org-clock-report
      "t" #'org-evaluate-time-range
      "=" #'org-clock-timestamps-up
      "-" #'org-clock-timestamps-down
      )
;;; <D>
(map! :after org
      :map org-mode-map
      :localleader
      :prefix ("d" . "date/deadline")
      "d" #'org-deadline
      "s" #'org-schedule
      "t" #'org-time-stamp
      "T" #'org-time-stamp-inactive
      )
;;; <G>
(map! :after org
      :map org-mode-map
      :localleader
      :prefix ("g" . "goto")
      "g" #'org-goto
      (:when (featurep! :completion ivy)
       "g" #'counsel-org-goto
       "G" #'counsel-org-goto-all)
      (:when (featurep! :completion helm)
       "g" #'helm-org-in-buffer-headings
       "G" #'helm-org-agenda-files-headings)
      "c" #'org-clock-goto
      "C" (cmd! (org-clock-goto 'select))
      "i" #'org-id-goto
      "r" #'org-refile-goto-last-stored
      "v" #'+org/goto-visible
      "x" #'org-capture-goto-last-stored
      )
;;; <I>
(map! :after org
      :map org-mode-map
      :localleader
      :prefix ("i" . "Insert")
      "t" #'org-todo
      "f" #'org-footnote-new
      "l" #'org-insert-link
      "S" #'org-insert-last-stored-link
      )
;;; <L>
(map! :after org
      :map org-mode-map
      :localleader
      :prefix ("l" . "links")
      "c" #'org-cliplink
      "d" #'+org/remove-link
      "i" #'org-id-store-link
      "l" #'org-insert-link
      "L" #'org-insert-all-links
      "s" #'org-store-link
      "S" #'org-insert-last-stored-link
      )
;;; <P>
(map! :after org
      :map org-mode-map
      :localleader
      :prefix ("P" . "publish")
      "a" #'org-publish-all
      "f" #'org-publish-current-file
      "p" #'org-publish
      "P" #'org-publish-current-project
      "s" #'org-publish-sitemap
      )
;;; <R>
(map! :after org
      :map org-mode-map
      :localleader
      :prefix ("r" . "refile")
      "." #'+org/refile-to-current-file
      "c" #'+org/refile-to-running-clock
      "l" #'+org/refile-to-last-location
      "f" #'+org/refile-to-file
      "o" #'+org/refile-to-other-window
      "O" #'+org/refile-to-other-buffer
      "v" #'+org/refile-to-visible
      "r" #'org-refile ; to all `org-refile-targets'
      )
;;; <S>
(map! :after org
      :map org-mode-map
      :localleader
      :prefix ("s" . "Tree/Subtree")
      "b" #'org-tree-to-indirect-buffer
      "d" #'org-cut-subtree
      "h" #'org-promote-subtree
      "j" #'org-move-subtree-down
      "k" #'org-move-subtree-up
      "l" #'org-demote-subtree
      "n" #'org-narrow-to-subtree
      "r" #'org-refile
      "s" #'org-sparse-tree
      "A" #'org-archive-subtree
      "N" #'widen
      "S" #'org-sort
      (:prefix ("p" . "Org Priority")
       "d" #'org-priority-down
       "p" #'org-priority
       "u" #'org-priority-up)
      )
;;; <T>
(map! :after org
      :map org-mode-map
      :localleader
      :prefix ("t" . "Toggle")
        "a" #'org-toggle-archive-tag
        "i" #'org-toggle-item
        "h" #'org-toggle-heading
        "I" #'org-toggle-inline-images
        "x" #'org-toggle-checkbox
        "l" #'org-toggle-link-display
        (:prefix ("t" . "table")
         "f" #'org-table-toggle-formula-debugger
         "o" #'org-table-toggle-coordinate-overlays)
        )
