;;; +agenda-bindings.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(defvar jg-org-agenda-mode-map (make-sparse-keymap))
(evil-make-overriding-map jg-org-agenda-mode-map)

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
     ;; :n "k"                 #'org-agenda-capture
     :n "DEL"                  #'org-agenda-show-scroll-down
     :n "RET"                  #'org-agenda-switch-to
     :n "SPC"                  #'org-agenda-show-and-scroll-up
     :n "TAB"                  #'org-agenda-goto
     :n "g"                    #'org-agenda-redo-all
     :n "q"                    #'org-agenda-quit
     :n "Q"                    #'org-agenda-exit
     :n "s"                    #'org-save-all-org-buffers
     :n "A"                    #'org-agenda-append-agenda

     :n "L"                    #'org-agenda-recenter
     :n "F"                    #'org-agenda-follow-mode
     :n "D"                    #'org-agenda-toggle-diary
     ;; :n "l"                 #'org-agenda-log-mode
      )

(map! :map jg-org-agenda-mode-map ;; movement
      :n "J"  #'org-agenda-backward-block
      :n "L"  #'org-agenda-forward-block
      :n "K"  #'org-agenda-drag-line-forward
      :n "J"  #'org-agenda-drag-line-backward
      :n "\\" #'org-agenda-end-of-line
      :n "j"  #'org-agenda-next-line
      :n "k"  #'org-agenda-previous-line
      :n "i"  #'org-agenda-diary-entry

      :n "c"  #'org-agenda-goto-calendar
      :n "/"  #'org-agenda-goto-date
      :n "."  #'org-agenda-goto-today

      :n "b"  #'org-agenda-earlier
      :n "f"  #'org-agenda-later
      :n "v"  #'org-agenda-view-mode-dispatch
      :n "o"  #'org-agenda-open-link
      )

(map! :map jg-org-agenda-mode-map ;; edit
     :n "u"                 #'org-agenda-undo
     :n "r"                 #'org-agenda-redo
     :n "$"                 #'org-agenda-archive
     :n "a"                 #'org-agenda-archive-default-with-confirmation
     :n "E"                 #'org-agenda-entry-text-mode
      )

(map! :map jg-org-agenda-mode-map ;; metadata
      :prefix ("c" . "metadata")
      ;;                  #'org-agenda-set-property

      :n "!"                 #'org-agenda-toggle-deadlines

      :n "T"                 #'org-agenda-show-tags
      :n ":"                 #'org-agenda-set-tags

      :n "-"                 #'org-agenda-priority-down
      :n ","                 #'org-agenda-priority
      :n "+"                 #'org-agenda-priority-up

      :n "t"                 #'org-agenda-todo
      :n ">"                 #'org-agenda-date-prompt

      :n "z"                 #'org-agenda-add-note
      :n "?"                 #'org-agenda-show-the-flagging-note

      :n "e"                 #'org-agenda-set-effort
      )

(map! :map jg-org-agenda-mode-map ;; filter
      :prefix ("f" . "filter")
      :n "["                 #'org-agenda-manipulate-query-add
      :n "]"                 #'org-agenda-manipulate-query-subtract
      :n "{"                 #'org-agenda-manipulate-query-add-re
      :n "}"                 #'org-agenda-manipulate-query-subtract-re
      :n "#"                 #'org-agenda-dim-blocked-tasks
      :n "d"                 #'org-agenda-day-view
      :n "/"                 #'org-agenda-filter
      :n "<"                 #'org-agenda-filter-by-category
      :n "="                 #'org-agenda-filter-by-regexp
      :n "|"                 #'org-agenda-filter-remove-all
      :n "\\"                #'org-agenda-filter-by-tag
      :n "^"                 #'org-agenda-filter-by-top-headline
      :n "_"                 #'org-agenda-filter-by-effort
      :n "~"                 #'org-agenda-limit-interactively
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
      :n "G"                 #'org-agenda-toggle-time-grid
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
      ;; #'org-attach
      ;; #'org-reload
      ;; #'org-agenda-refile
      ;; #'org-agenda-columns
      ;; #'org-mobile-pull
      ;; #'org-agenda-tree-to-indirect-buffer
      ;; #'org-mobile-push
      ;; #'org-info-find-node
      ;; #'org-agenda-kill
      ;; #'org-save-all-org-buffers
      ;; #'org-agenda-write
     ;; :n "k"                 #'org-agenda-capture
     :n "DEL"              #'org-agenda-show-scroll-down
     :n "RET"              #'org-agenda-switch-to
     :n "SPC"              #'org-agenda-show-and-scroll-up
     :n "TAB"              #'org-agenda-goto
     :n "g"                 #'org-agenda-redo-all
     :n "q"                 #'org-agenda-quit
     :n "Q"                 #'org-agenda-exit
     :n "s"                 #'org-save-all-org-buffers
     :n "A"                 #'org-agenda-append-agenda

     :n "L"                 #'org-agenda-recenter
     :n "F"                 #'org-agenda-follow-mode
     :n "D"                 #'org-agenda-toggle-diary
     ;; :n "l"                 #'org-agenda-log-mode
      )

(map! :map jg-org-agenda-mode-map ;; movement
      :localleader
      :n "J"  #'org-agenda-backward-block
      :n "L"  #'org-agenda-forward-block
      :n "K"  #'org-agenda-drag-line-forward
      :n "J"  #'org-agenda-drag-line-backward
      :n "\\" #'org-agenda-end-of-line
      :n "j"  #'org-agenda-next-line
      :n "k"  #'org-agenda-previous-line
      :n "i"  #'org-agenda-diary-entry

      :n "c"  #'org-agenda-goto-calendar
      :n "/"  #'org-agenda-goto-date
      :n "."  #'org-agenda-goto-today

      :n "b"  #'org-agenda-earlier
      :n "f"  #'org-agenda-later
      :n "v"  #'org-agenda-view-mode-dispatch
      :n "o"  #'org-agenda-open-link
      )

(map! :map jg-org-agenda-mode-map ;; edit
      :localleader
     :n "u"                 #'org-agenda-undo
     :n "r"                 #'org-agenda-redo
     :n "$"                 #'org-agenda-archive
     :n "a"                 #'org-agenda-archive-default-with-confirmation
     :n "E"                 #'org-agenda-entry-text-mode
      )

(map! :map jg-org-agenda-mode-map ;; metadata
      :localleader
      :prefix ("c" . "metadata")
      ;;                  #'org-agenda-set-property

      :n "!"                 #'org-agenda-toggle-deadlines

      :n "T"                 #'org-agenda-show-tags
      :n ":"                 #'org-agenda-set-tags

      :n "-"                 #'org-agenda-priority-down
      :n ","                 #'org-agenda-priority
      :n "+"                 #'org-agenda-priority-up

      :n "t"                 #'org-agenda-todo
      :n ">"                 #'org-agenda-date-prompt

      :n "z"                 #'org-agenda-add-note
      :n "?"                 #'org-agenda-show-the-flagging-note

      :n "e"                 #'org-agenda-set-effort
      )

(map! :map jg-org-agenda-mode-map ;; filter
      :localleader
      :prefix ("f" . "filter")
      :n "["                 #'org-agenda-manipulate-query-add
      :n "]"                 #'org-agenda-manipulate-query-subtract
      :n "{"                 #'org-agenda-manipulate-query-add-re
      :n "}"                 #'org-agenda-manipulate-query-subtract-re
      :n "#"                 #'org-agenda-dim-blocked-tasks
      :n "d"                 #'org-agenda-day-view
      :n "/"                 #'org-agenda-filter
      :n "<"                 #'org-agenda-filter-by-category
      :n "="                 #'org-agenda-filter-by-regexp
      :n "|"                 #'org-agenda-filter-remove-all
      :n "\\"                #'org-agenda-filter-by-tag
      :n "^"                 #'org-agenda-filter-by-top-headline
      :n "_"                 #'org-agenda-filter-by-effort
      :n "~"                 #'org-agenda-limit-interactively
      )

(map! :map jg-org-agenda-mode-map ;; time
      :localleader
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
      :n "G"                 #'org-agenda-toggle-time-grid
      :n "J"                 #'org-agenda-clock-goto
      :n "I"                 #'org-agenda-clock-in
      :n "O"                 #'org-agenda-clock-out
      :n "R"                 #'org-agenda-clockreport-mode
      :n "X"                 #'org-agenda-clock-cancel
      :n "C"                 #'org-agenda-convert-date
      )

(map! :map jg-org-agenda-mode-map ;; mark
      :localleader
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

;;-- end localleader

(after! (evil-org org)
  (setq org-agenda-mode-map jg-org-agenda-mode-map)
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
