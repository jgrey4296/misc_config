;;; lang/org/+bindings.el -*- lexical-binding: t; -*-
;;; Misc
;; (map! :map jg-org-mode-map
;;       ;; textmate-esque newline insertion
;;       "S-RET"      #'+org/shift-return
;;       "C-RET"      #'+org/insert-item-below
;;       "C-S-RET"    #'+org/insert-item-above
;;       "C-M-RET"    #'org-insert-subheading
;;       [C-return]   #'+org/insert-item-below
;;       [C-S-return] #'+org/insert-item-above
;;       [C-M-return] #'org-insert-subheading
;;       (:when IS-MAC
;;        [s-return]   #'+org/insert-item-below
;;        [s-S-return] #'+org/insert-item-above
;;        [s-M-return] #'org-insert-subheading)
;;       ;; Org-aware C-a/C-e
;;       [remap doom/backward-to-bol-or-indent]          #'org-beginning-of-line
;;       [remap doom/forward-to-last-non-comment-or-eol] #'org-end-of-line

;;       )
;; ;;; Misc 2
;; (map! :map jg-org-mode-map
;;       :localleader
;;       "#" #'org-update-statistics-cookies
;;       "'" #'org-edit-special
;;       "*" #'org-ctrl-c-star
;;       "," #'org-switchb
;;       "." #'org-goto
;;       (:when (featurep! :completion ivy)
;;        "." #'counsel-org-goto
;;        "/" #'counsel-org-goto-all)
;;       (:when (featurep! :completion helm)
;;        "." #'helm-org-in-buffer-headings
;;        "/" #'helm-org-agenda-files-headings)
;;       )

;; ;;; No Prefix
;; (map! :map jg-org-mode-map
;;       "A" #'org-archive-subtree
;;       "e" #'org-export-dispatch
;;       "f" #'org-footnote-new
;;       "h" #'org-toggle-heading
;;       "i" #'org-toggle-item
;;       "I" #'org-toggle-inline-images
;;       "n" #'org-store-link
;;       "o" #'org-set-property
;;       "p" #'org-priority
;;       "q" #'org-set-tags-command
;;       "t" #'org-todo
;;       "T" #'org-todo-list
;;       "x" #'org-toggle-checkbox

;;       )
;; ;;; <A>
;; (map! :map jg-org-mode-map
;;       :prefix ("a" . "attachments")
;;        "a" #'org-attach
;;        "d" #'org-attach-delete-one
;;        "D" #'org-attach-delete-all
;;        "f" #'+org/find-file-in-attachments
;;        "l" #'+org/attach-file-and-insert-link
;;        "n" #'org-attach-new
;;        "o" #'org-attach-open
;;        "O" #'org-attach-open-in-emacs
;;        "r" #'org-attach-reveal
;;        "R" #'org-attach-reveal-in-emacs
;;        "u" #'org-attach-url
;;        "s" #'org-attach-set-directory
;;        "S" #'org-attach-sync
;;        (:when (featurep! +dragndrop)
;;         "c" #'org-download-screenshot
;;         "p" #'org-download-clipboard
;;         "P" #'org-download-yank)
;;       )

;; ;;; <B>
;; (map! :map jg-org-mode-map
;;       :prefix ("b" . "tables")
;;        "-" #'org-table-insert-hline
;;        "a" #'org-table-align
;;        "b" #'org-table-blank-field
;;        "c" #'org-table-create-or-convert-from-region
;;        "e" #'org-table-edit-field
;;        "f" #'org-table-edit-formulas
;;        "h" #'org-table-field-info
;;        "s" #'org-table-sort-lines
;;        "r" #'org-table-recalculate
;;        "R" #'org-table-recalculate-buffer-tables
;;        (:prefix ("d" . "delete")
;;         "c" #'org-table-delete-column
;;         "r" #'org-table-kill-row)
;;        (:prefix ("i" . "insert")
;;         "c" #'org-table-insert-column
;;         "h" #'org-table-insert-hline
;;         "r" #'org-table-insert-row
;;         "H" #'org-table-hline-and-move)
;;        (:prefix ("t" . "toggle")
;;         "f" #'org-table-toggle-formula-debugger
;;         "o" #'org-table-toggle-coordinate-overlays)
;;        (:when (featurep! +gnuplot)
;;         "p" #'org-plot/gnuplot)
;;       )
;; ;;; <C>
;; (map! :map jg-org-mode-map
;;       (:prefix ("c" . "clock")
;;        "c" #'org-clock-cancel
;;        "d" #'org-clock-mark-default-task
;;        "e" #'org-clock-modify-effort-estimate
;;        "E" #'org-set-effort
;;        "g" #'org-clock-goto
;;        "G" (cmd! (org-clock-goto 'select))
;;        "l" #'+org/toggle-last-clock
;;        "i" #'org-clock-in
;;        "I" #'org-clock-in-last
;;        "o" #'org-clock-out
;;        "r" #'org-resolve-clocks
;;        "R" #'org-clock-report
;;        "t" #'org-evaluate-time-range
;;        "=" #'org-clock-timestamps-up
;;        "-" #'org-clock-timestamps-down)
;;       )
;; ;;; <D>
;; (map! :map jg-org-mode-map
;;       (:prefix ("d" . "date/deadline")
;;        "d" #'org-deadline
;;        "s" #'org-schedule
;;        "t" #'org-time-stamp
;;        "T" #'org-time-stamp-inactive)
;;       )
;; ;;; <G>
;; (map! :map jg-org-mode-map
;;       (:prefix ("g" . "goto")
;;        "g" #'org-goto
;;        (:when (featurep! :completion ivy)
;;         "g" #'counsel-org-goto
;;         "G" #'counsel-org-goto-all)
;;        (:when (featurep! :completion helm)
;;         "g" #'helm-org-in-buffer-headings
;;         "G" #'helm-org-agenda-files-headings)
;;        "c" #'org-clock-goto
;;        "C" (cmd! (org-clock-goto 'select))
;;        "i" #'org-id-goto
;;        "r" #'org-refile-goto-last-stored
;;        "v" #'+org/goto-visible
;;        "x" #'org-capture-goto-last-stored)
;;       )
;; ;;; <L>
;; (map! :map jg-org-mode-map
;;       (:prefix ("l" . "links")
;;        "c" #'org-cliplink
;;        "d" #'+org/remove-link
;;        "i" #'org-id-store-link
;;        "l" #'org-insert-link
;;        "L" #'org-insert-all-links
;;        "s" #'org-store-link
;;        "S" #'org-insert-last-stored-link
;;        "t" #'org-toggle-link-display)

;;       )
;; ;;; <P>
;; (map! :map jg-org-mode-map
;;       (:prefix ("P" . "publish")
;;        "a" #'org-publish-all
;;        "f" #'org-publish-current-file
;;        "p" #'org-publish
;;        "P" #'org-publish-current-project
;;        "s" #'org-publish-sitemap)
;;       )
;; ;;; <R>
;; (map! :map jg-org-mode-map
;;       (:prefix ("r" . "refile")
;;        "." #'+org/refile-to-current-file
;;        "c" #'+org/refile-to-running-clock
;;        "l" #'+org/refile-to-last-location
;;        "f" #'+org/refile-to-file
;;        "o" #'+org/refile-to-other-window
;;        "O" #'+org/refile-to-other-buffer
;;        "v" #'+org/refile-to-visible
;;        "r" #'org-refile) ; to all `org-refile-targets'
;;       )
;; ;;; <S>
;; (map! :map jg-org-mode-map
;;       (:prefix ("s" . "Tree/Subtree")
;;        "a" #'org-toggle-archive-tag
;;        "b" #'org-tree-to-indirect-buffer
;;        "d" #'org-cut-subtree
;;        "h" #'org-promote-subtree
;;        "j" #'org-move-subtree-down
;;        "k" #'org-move-subtree-up
;;        "l" #'org-demote-subtree
;;        "n" #'org-narrow-to-subtree
;;        "r" #'org-refile
;;        "s" #'org-sparse-tree
;;        "A" #'org-archive-subtree
;;        "N" #'widen
;;        "S" #'org-sort
;;        (:prefix ("p" . "Org Priority")
;;         "d" #'org-priority-down
;;         "p" #'org-priority
;;         "u" #'org-priority-up)))
;;; Agenda
(map! :after org-agenda
      :map org-agenda-mode-map
      :m "C-SPC" #'org-agenda-show-and-scroll-up
      :localleader
      (:prefix ("d" . "date/deadline")
       "d" #'org-agenda-deadline
       "s" #'org-agenda-schedule)
      (:prefix ("c" . "clock")
       "c" #'org-agenda-clock-cancel
       "g" #'org-agenda-clock-goto
       "i" #'org-agenda-clock-in
       "o" #'org-agenda-clock-out
       "r" #'org-agenda-clockreport-mode
       "s" #'org-agenda-show-clocking-issues)
      "q" #'org-agenda-set-tags
      "r" #'org-agenda-refile
      "t" #'org-agenda-todo)


(map! :map evil-org-mode-map
      :ni [C-return]   #'+org/insert-item-below
      :ni [C-S-return] #'+org/insert-item-above
      ;; navigate table cells (from insert-mode)
      :i  "C-l"     (cmds! (org-at-table-p) #'org-table-next-field
                           #'org-end-of-line)
      :i  "C-h"     (cmds! (org-at-table-p) #'org-table-previous-field
                           #'org-beginning-of-line)
      :i  "C-k"     (cmds! (org-at-table-p) #'+org/table-previous-row
                           #'org-up-element)
      :i  "C-j"     (cmds! (org-at-table-p) #'org-table-next-row
                           #'org-down-element)
      :ni "C-S-l"   #'org-shiftright
      :ni "C-S-h"   #'org-shiftleft
      :ni "C-S-k"   #'org-shiftup
      :ni "C-S-j"   #'org-shiftdown

      ;;:n "TAB" #'org-cycle-internal-local
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
      :n "gQ"  #'org-fill-paragraph
      ;; sensible vim-esque folding keybinds
      :n "za"  #'+org/toggle-fold
      :n "zA"  #'org-shifttab
      :n "zc"  #'+org/close-fold
      :n "zC"  #'outline-hide-subtree
      :n "zm"  #'+org/hide-next-fold-level
      :n "zM"  #'+org/close-all-folds
      :n "zn"  #'org-tree-to-indirect-buffer
      :n "zo"  #'+org/open-fold
      :n "zO"  #'outline-show-subtree
      :n "zr"  #'+org/show-next-fold-level
      :n "zR"  #'+org/open-all-folds
      :n "zi"  #'org-toggle-inline-images

      :map org-read-date-minibuffer-local-map
      "C-h"   (cmd! (org-eval-in-calendar '(calendar-backward-day 1)))
      "C-l"   (cmd! (org-eval-in-calendar '(calendar-forward-day 1)))
      "C-k"   (cmd! (org-eval-in-calendar '(calendar-backward-week 1)))
      "C-j"   (cmd! (org-eval-in-calendar '(calendar-forward-week 1)))
      "C-S-h" (cmd! (org-eval-in-calendar '(calendar-backward-month 1)))
      "C-S-l" (cmd! (org-eval-in-calendar '(calendar-forward-month 1)))
      "C-S-k" (cmd! (org-eval-in-calendar '(calendar-backward-year 1)))
      "C-S-j" (cmd! (org-eval-in-calendar '(calendar-forward-year 1))))
(map! :map org-mode-map
        "C-c C-S-l"  #'+org/remove-link
        "C-c C-i"    #'org-toggle-inline-images
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

        :localleader
        "#" #'org-update-statistics-cookies
        "'" #'org-edit-special
        "*" #'org-ctrl-c-star
        "+" #'org-ctrl-c-minus
        "," #'org-switchb
        "." #'org-goto
        (:when (featurep! :completion ivy)
         "." #'counsel-org-goto
         "/" #'counsel-org-goto-all)
        (:when (featurep! :completion helm)
         "." #'helm-org-in-buffer-headings
         "/" #'helm-org-agenda-files-headings)
        "A" #'org-archive-subtree
        "e" #'org-export-dispatch
        "f" #'org-footnote-new
        "h" #'org-toggle-heading
        "i" #'org-toggle-item
        "I" #'org-toggle-inline-images
        "n" #'org-store-link
        "o" #'org-set-property
        "p" #'org-priority
        "q" #'org-set-tags-command
        "t" #'org-todo
        "T" #'org-todo-list
        "x" #'org-toggle-checkbox
        (:prefix ("a" . "attachments")
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
          "P" #'org-download-yank))
        (:prefix ("b" . "tables")
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
         (:prefix ("t" . "toggle")
          "f" #'org-table-toggle-formula-debugger
          "o" #'org-table-toggle-coordinate-overlays)
         (:when (featurep! +gnuplot)
          "p" #'org-plot/gnuplot))
        (:prefix ("c" . "clock")
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
         "-" #'org-clock-timestamps-down)
        (:prefix ("d" . "date/deadline")
         "d" #'org-deadline
         "s" #'org-schedule
         "t" #'org-time-stamp
         "T" #'org-time-stamp-inactive)
        (:prefix ("g" . "goto")
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
         "x" #'org-capture-goto-last-stored)
        (:prefix ("l" . "links")
         "c" #'org-cliplink
         "d" #'+org/remove-link
         "i" #'org-id-store-link
         "l" #'org-insert-link
         "L" #'org-insert-all-links
         "s" #'org-store-link
         "S" #'org-insert-last-stored-link
         "t" #'org-toggle-link-display)
        (:prefix ("P" . "publish")
         "a" #'org-publish-all
         "f" #'org-publish-current-file
         "p" #'org-publish
         "P" #'org-publish-current-project
         "s" #'org-publish-sitemap)
        (:prefix ("r" . "refile")
         "." #'+org/refile-to-current-file
         "c" #'+org/refile-to-running-clock
         "l" #'+org/refile-to-last-location
         "f" #'+org/refile-to-file
         "o" #'+org/refile-to-other-window
         "O" #'+org/refile-to-other-buffer
         "v" #'+org/refile-to-visible
         "r" #'org-refile) ; to all `org-refile-targets'
        (:prefix ("s" . "Tree/Subtree")
         "a" #'org-toggle-archive-tag
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
          "u" #'org-priority-up)))
(map! :after org-agenda
        :map org-agenda-mode-map
        :m "C-SPC" #'org-agenda-show-and-scroll-up
        :localleader
        (:prefix ("d" . "date/deadline")
         "d" #'org-agenda-deadline
         "s" #'org-agenda-schedule)
        (:prefix ("c" . "clock")
         "c" #'org-agenda-clock-cancel
         "g" #'org-agenda-clock-goto
         "i" #'org-agenda-clock-in
         "o" #'org-agenda-clock-out
         "r" #'org-agenda-clockreport-mode
         "s" #'org-agenda-show-clocking-issues)
        "q" #'org-agenda-set-tags
        "r" #'org-agenda-refile
        "t" #'org-agenda-todo)
