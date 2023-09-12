;; lang/org/+bindings_2.el -*- lexical-binding: t; -*-
;; Misc
(doom-log "Setting up org main bindings: %s" (current-time-string))

;; Leaderless
(map! :map jg-org-mode-map
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
      [tab]        #'org-cycle
      )
;; Local Leader
(map! :map jg-org-mode-map
      :localleader
      :desc "update-statistics-cookies" "#" #'org-update-statistics-cookies
      :desc "edit-special"              "'" #'org-edit-special
      :desc "ctrl-c-star"               "*" #'org-ctrl-c-star
      :desc "switchb"                   "," #'org-switchb

      :desc "archive-subtree"           "A" #'org-archive-subtree
      :desc "export-dispatch"           "e" #'org-export-dispatch
      :desc "store-link"                "n" #'org-store-link
      :desc "set-property"              "o" #'org-set-property
      :desc "set-tags-command"          "q" #'org-set-tags-command
      :desc "todo-list"                 "T" #'org-todo-list
      )
;; <A> Attachments
(map! :map jg-org-mode-map
      :localleader
      :prefix ("a" . "attachments")
      :desc "attach"                            "a" #'org-attach
      :desc "attach-delete-one"                 "d" #'org-attach-delete-one
      :desc "attach-delete-all"                 "D" #'org-attach-delete-all
      :desc "find-file-in-attachments"     "f" #'+org/find-file-in-attachments
      :desc "attach-file-and-insert-link"  "l" #'+org/attach-file-and-insert-link
      :desc "attach-new"                        "n" #'org-attach-new
      :desc "attach-open"                       "o" #'org-attach-open
      :desc "attach-open-in-emacs"              "O" #'org-attach-open-in-emacs
      :desc "attach-reveal"                     "r" #'org-attach-reveal
      :desc "attach-reveal-in-emacs"            "R" #'org-attach-reveal-in-emacs
      :desc "attach-url"                        "u" #'org-attach-url
      :desc "attach-set-directory"              "s" #'org-attach-set-directory
      :desc "attach-sync"                       "S" #'org-attach-sync
      )
;; <B> Tables
(map! :map jg-org-mode-map
      :localleader
      :prefix ("b" . "tables")
      :desc "table-insert-hline"                   "-" #'org-table-insert-hline
      :desc "table-align"                          "a" #'org-table-align
      :desc "table-blank-field"                    "b" #'org-table-blank-field
      :desc "table-create-or-convert-from-region"  "c" #'org-table-create-or-convert-from-region
      :desc "table-edit-field"                     "e" #'org-table-edit-field
      :desc "table-edit-formulas"                  "f" #'org-table-edit-formulas
      :desc "table-field-info"                     "h" #'org-table-field-info
      :desc "table-sort-lines"                     "s" #'org-table-sort-lines
      :desc "table-recalculate"                    "r" #'org-table-recalculate
      :desc "table-recalculate-buffer-tables"      "R" #'org-table-recalculate-buffer-tables
      (:prefix ("d" . "delete")
       :desc "table-delete-column"                 "c" #'org-table-delete-column
       :desc "table-kill-row)"                     "r" #'org-table-kill-row)
      (:prefix ("i" . "insert")
       :desc "table-insert-column"                 "c" #'org-table-insert-column
       :desc "table-insert-hline"                  "h" #'org-table-insert-hline
       :desc "table-insert-row"                    "r" #'org-table-insert-row
       :desc "table-hline-and-move)"               "H" #'org-table-hline-and-move)
       :desc "plot/gnuplot"                        "p" #'org-plot/gnuplot
      )
;; <C> Clock
(map! :map jg-org-mode-map
      :localleader
      :prefix ("c" . "clock")
      :desc "clock-cancel"                  "c" #'org-clock-cancel
      :desc "clock-mark-default-task"       "d" #'org-clock-mark-default-task
      :desc "clock-modify-effort-estimate"  "e" #'org-clock-modify-effort-estimate
      :desc "set-effort"                    "E" #'org-set-effort
      :desc "clock-goto"                    "g" #'org-clock-goto
      :desc "Goto Clock"                    "G" (cmd! (org-clock-goto 'select))
      :desc "+org/toggle-last-clock"        "l" #'+org/toggle-last-clock
      :desc "clock-in"                      "i" #'org-clock-in
      :desc "clock-in-last"                 "I" #'org-clock-in-last
      :desc "clock-out"                     "o" #'org-clock-out
      :desc "resolve-clocks"                "r" #'org-resolve-clocks
      :desc "clock-report"                  "R" #'org-clock-report
      :desc "evaluate-time-range"           "t" #'org-evaluate-time-range
      :desc "clock-timestamps-up"           "=" #'org-clock-timestamps-up
      :desc "clock-timestamps-down"         "-" #'org-clock-timestamps-down
      )
;; <D> Dates
(map! :map jg-org-mode-map
      :localleader
      :prefix ("d" . "date/deadline")
      :desc "deadline"             "d" #'org-deadline
      :desc "schedule"             "s" #'org-schedule
      :desc "time-stamp"           "t" #'org-time-stamp
      :desc "time-stamp-inactive"  "T" #'org-time-stamp-inactive
      )
;; <F> Formatting
(map! :map jg-org-mode-map
      :localleader
      :prefix ("f" . "Format")

      )
;; <G> Goto
(map! :map jg-org-mode-map
      :localleader
      :prefix ("g" . "goto")
      :desc "goto"                              "g" #'org-goto
      :desc "counsel-org-goto"                  "g" #'counsel-org-goto
      :desc "counsel-org-goto-all"              "G" #'counsel-org-goto-all
      :desc "clock-goto"                        "c" #'org-clock-goto
      :desc "Select Clocked in Task"            "C" (cmd! (org-clock-goto 'select))
      :desc "id-goto"                           "i" #'org-id-goto
      :desc "refile-goto-last-stored"           "r" #'org-refile-goto-last-stored
      :desc "+org/goto-visible"                 "v" #'+org/goto-visible
      :desc "capture-goto-last-stored"          "x" #'org-capture-goto-last-stored
      )
;; <I> Insert
(map! :map jg-org-mode-map
      :localleader
      :prefix ("i" . "Insert")
      :desc "todo"                     "T" #'org-todo
      :desc "footnote-new"             "f" #'org-footnote-new
      :desc "insert-link"              "l" #'org-insert-link
      :desc "insert-last-stored-link"  "S" #'org-insert-last-stored-link
      :desc "insert citation"          "@" #'org-cite-insert
      :desc "insert new id"            "I" #'org-id-get-create
      )
;; <L> Links
(map! :map jg-org-mode-map
      :localleader
      :prefix ("l" . "links")
      :desc "cliplink"                 "c" #'org-cliplink
      :desc "+org/remove-link"         "d" #'+org/remove-link
      :desc "id-store-link"            "i" #'org-id-store-link
      :desc "insert-link"              "l" #'org-insert-link
      :desc "insert-all-links"         "L" #'org-insert-all-links
      :desc "store-link"               "s" #'org-store-link
      :desc "insert-last-stored-link"  "S" #'org-insert-last-stored-link
      )
;; <P> Publish
(map! :map jg-org-mode-map
      :localleader
      :prefix ("P" . "publish")
      :desc "publish-all"              "a" #'org-publish-all
      :desc "publish-current-file"     "f" #'org-publish-current-file
      :desc "publish"                  "p" #'org-publish
      :desc "publish-current-project"  "P" #'org-publish-current-project
      :desc "publish-sitemap"          "s" #'org-publish-sitemap
      )
;; <R> Refile
(map! :map jg-org-mode-map
      :localleader
      :prefix ("r" . "refile")
      :desc "refile-to-current-file"   "." #'+org/refile-to-current-file
      :desc "refile-to-running-clock"  "c" #'+org/refile-to-running-clock
      :desc "refile-to-last-location"  "l" #'+org/refile-to-last-location
      :desc "refile-to-file"           "f" #'+org/refile-to-file
      :desc "refile-to-other-window"   "o" #'+org/refile-to-other-window
      :desc "refile-to-other-buffer"   "O" #'+org/refile-to-other-buffer
      :desc "refile-to-visible"        "v" #'+org/refile-to-visible
      :desc "refile"                   "r" #'org-refile ; to all `org-refile-targets'
      )
;; <S> Subtree
(map! :map jg-org-mode-map
      :localleader
      :prefix ("s" . "Tree/Subtree")
      :desc "tree-to-indirect-buffer"             "b" #'org-tree-to-indirect-buffer
      :desc "cut-subtree"                         "d" #'org-cut-subtree
      :desc "promote-subtree"                     "h" #'org-promote-subtree
      :desc "subtree-down"                        "j" #'org-move-subtree-down
      :desc "subtree-up"                          "k" #'org-move-subtree-up
      :desc "demote-subtree"                      "l" #'org-demote-subtree
      :desc "narrow-to-subtree"                   "n" #'org-narrow-to-subtree
      :desc "refile"                              "r" #'org-refile
      :desc "sparse-tree"                         "s" #'org-sparse-tree
      :desc "archive-subtree"                     "A" #'org-archive-subtree
      :desc "sort"                                "S" #'org-sort
      (:prefix ("p" . "Org Priority")
       :desc "priority-down"                      "j" #'org-priority-down
       :desc "priority"                           "p" #'org-priority
       :desc "priority-up"                        "k" #'org-priority-up)
      )
;; <T> Toggle
(map! :map jg-org-mode-map
      :localleader
      :prefix ("t" . "Toggle")
      :desc "archive-tag"                  "a" #'org-toggle-archive-tag
      :desc "item"                         "i" #'org-toggle-item
      :desc "heading"                      "h" #'org-toggle-heading
      :desc "inline-images"                "I" #'org-toggle-inline-images
      :desc "checkbox"                     "x" #'org-toggle-checkbox
      :desc "link-display"                 "l" #'org-toggle-link-display
      (:prefix ("t" . "table")
       :desc "formula-debugger"      "f" #'org-table-toggle-formula-debugger
       :desc "coordinate-overlays"  "o" #'org-table-toggle-coordinate-overlays)
      )

(map! :map jg-org-mode-map
      :ni [C-return]   #'+org/insert-item-below
      :ni [C-S-return] #'+org/insert-item-above
      ;; more intuitive RET keybinds
      :n [return]   #'+org/dwim-at-point
      :n "RET"      #'+org/dwim-at-point
      :i [return]   (cmd! (org-return electric-indent-mode))
      :i "RET"      (cmd! (org-return electric-indent-mode))
      :i [S-return] #'+org/shift-return
      :i "S-RET"    #'+org/shift-return
      ;; more vim-esque org motion keys (not covered by evil-org-mode)
      :m "]h"  #'org-forward-heading-same-level
      :m "[h"  #'org-backward-heading-same-level
      :m "]l"  #'org-next-link
      :m "[l"  #'org-previous-link
      :m "]c"  #'org-babel-next-src-block
      :m "[c"  #'org-babel-previous-src-block
      :n "zn"  #'org-tree-to-indirect-buffer
      )
