;;; +agenda-bindings.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(defvar-keymap jg-org-agenda-mode-map)
(defvar-keymap jg-org-agenda-diary-keymap)

(define-prefix-command 'jg-org-agenda-meta-submap nil "jgoa-meta-map")
(set-keymap-parent jg-org-agenda-meta-submap jg-binding-halting-keymap)
(evil-make-overriding-map jg-org-agenda-meta-submap)

(map! :map jg-org-agenda-mode-map ;; main
      ;;                       #'org-attach
      ;;                       #'org-reload
      ;;                       #'org-agenda-refile
      ;;                       #'org-agenda-columns
      ;;                       #'org-mobile-pull
      ;;                       #'org-agenda-tree-to-indirect-buffer
      ;;                       #'org-mobile-push
      ;;                       #'org-info-find-node
      ;;                       #'org-agenda-kill
      ;;                       #'org-save-all-org-buffers
      ;;                       #'org-agenda-write
     ;;                            :n "k"                 #'org-agenda-capture
     :desc "Switch to file"        :n "RET"                  #'org-agenda-switch-to
     :desc "Goto File"             :n "TAB"                  #'org-agenda-goto
     :desc "Rebuild Agenda"        :n "g"                    #'org-agenda-redo-all
                                   :n "Q"                    #'org-agenda-quit
                                   :n "q"                    #'org-agenda-exit
                                   :n "a"                    #'org-agenda-append-agenda
     :desc "Metadata"              :n "c"                     'jg-org-agenda-meta-submap
     :desc "Filter by Agenda File" :n "/"                    #'org-agenda-filter

                                   :n "b"                    #'org-agenda-dim-blocked-tasks
     ;;                            :n "l"                 #'org-agenda-log-mode

      )

(map! :map jg-org-agenda-mode-map ;; movement
      ;;                   :n "J"  #'org-agenda-backward-block
      ;;                   :n "L"  #'org-agenda-forward-block
      :desc "Drag Down"    :n "J"  #'org-agenda-drag-line-forward
      :desc "Drag Up"      :n "K"  #'org-agenda-drag-line-backward
      :desc "Insert"       :n "i"  #'org-agenda-diary-entry

      :desc "Calendar"     :n "s c"  #'org-agenda-goto-calendar
                           :n "/"  #'org-agenda-goto-date
                           :n "."  #'org-agenda-goto-today

      :desc "Earlier"      :n "h"  #'org-agenda-earlier
      :desc "Later"        :n "l"  #'org-agenda-later
      :desc "View Control" :n "v"  #'org-agenda-view-mode-dispatch
                           ;; :n "o"  #'org-agenda-open-link
      )

(map! :map jg-org-agenda-mode-map ;; filter
      :prefix ("f" . "filter")
      ;; :desc "Add"                     :n "a"                 #'org-agenda-manipulate-query-add
      ;; :desc "Remove"                  :n "r"                 #'org-agenda-manipulate-query-subtract
      ;; :desc "Add Regex"               :n "{"                 #'org-agenda-manipulate-query-add-re
      ;; :desc "Sub Regex"               :n "}"                 #'org-agenda-manipulate-query-subtract-re
      :desc "By Current Category"     :n "l"                 #'org-agenda-filter-by-category
      :desc "By Regexp"               :n "r"                 #'org-agenda-filter-by-regexp
      :desc "Clear Filters"           :n "f"                 #'org-agenda-filter-remove-all
      :desc "By Tag"                  :n "t"                 #'org-agenda-filter-by-tag
      :desc "Subtrees"                :n "s"                 #'org-agenda-filter-by-top-headline
      :desc "By Effort"               :n "e"                 #'org-agenda-filter-by-effort
      :desc "By Number"               :n "n"                 #'org-agenda-limit-interactively
      )

(map! :map jg-org-agenda-mode-map ;; time
      :prefix ("t" . "time")
      ;; #'org-agenda-schedule
      ;; #'org-agenda-deadline
      ;; #'org-agenda-do-date-earlier
      ;; #'org-agenda-do-date-later
      ;; #'org-timer-stop
      ;; #'org-clock-modify-effort-estimate

      :n "S"                 #'org-agenda-sunrise-sunset
      :n "M"                 #'org-agenda-phases-of-moon
      :n "H"                 #'org-agenda-holidays
      :n "y"                 #'org-agenda-year-view
      :n "w"                 #'org-agenda-week-view
      :n "h"                 #'org-agenda-holidays
      :n ";"                 #'org-timer-set-timer
      :n "J"                 #'org-agenda-clock-goto
      :n "I"                 #'org-agenda-clock-in
      :n "O"                 #'org-agenda-clock-out
      :n "R"                 #'org-agenda-clockreport-mode
      :n "X"                 #'org-agenda-clock-cancel
      :n "C"                 #'org-agenda-convert-date
      )

(map! :map jg-org-agenda-mode-map ;; mark
      :prefix ("m" . "mark")
      :n "t"                 #'org-agenda-bulk-toggle
      :n "T"                 #'org-agenda-bulk-toggle-all
      :n "%"                 #'org-agenda-bulk-mark-regexp
      :n "m"                 #'org-agenda-bulk-mark
      :n "u"                 #'org-agenda-bulk-unmark
      :n "U"                 #'org-agenda-bulk-unmark-all
      :n "*"                 #'org-agenda-bulk-mark-all
      :n "B"                 #'org-agenda-bulk-action
      )

;;-- localleader
(map! :map jg-org-agenda-mode-map ;; main
      :localleader
      :desc "Rebuild Agenda"     :n "g"                 #'org-agenda-redo-all
      :n "Q"                 #'org-agenda-quit
      :n "q"                 #'org-agenda-exit

      :desc "Save org buffers" :n "s"                 #'org-save-all-org-buffers
      :desc "Append agenda" :n "a"                 #'org-agenda-append-agenda

      :n "L"                 #'org-agenda-recenter
      )

(map! :map jg-org-agenda-mode-map ;; toggles
      :localleader
      :prefix ("t" . "Toggle")

      :desc "Toggle Diary" :n "d"                    #'org-agenda-toggle-diary
      :desc "Follow Mode"  :n "f"                    #'org-agenda-follow-mode
      :desc "Deadlines"    :n "!"                    #'org-agenda-toggle-deadlines
      :desc "Time Grid"    :n "g"                 #'org-agenda-toggle-time-grid
      )

(map! :map jg-org-agenda-mode-map
      :localleader
      :prefix ("a" . "Archive")
      :desc "Entry"         :n "e"                 #'org-agenda-archive
      :desc "Default Entry" :n "E"                 #'org-agenda-archive-default-with-confirmation
      )

;;-- end localleader

(map! :map jg-org-agenda-meta-submap
      ;;                  #'org-agenda-set-property
      (:prefix ("t" . "Tags")
       :desc "List tags"  "l" #'org-agenda-show-tags
       :desc "Set Tags"   "s" #'org-agenda-set-tags
       )
      (:prefix ("p" . "Priority")
        "k" #'org-agenda-priority-down
        "l" #'org-agenda-priority
        "j" #'org-agenda-priority-up
        )
      :desc "Todo" "TAB"     #'org-agenda-todo
      :desc "Set Effort" "e" #'org-agenda-set-effort
      ;; "z"                 #'org-agenda-add-note
      ;; "?"                 #'org-agenda-show-the-flagging-note

      )


(after! (evil-org org)
  (setq org-agenda-mode-map jg-org-agenda-mode-map
        diary-mode-map      jg-org-agenda-diary-keymap
        )
  )

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    May 26, 2024
;; Modified:   May 26, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; +agenda-bindings.el ends here
