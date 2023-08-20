;;; +bindings.el -*- lexical-binding: t; no-byte-compile: t; -*-

(doom-log "Setting up Ibuffer bindings: %s" (current-time-string))

(map! :leader
      :desc "Ibuffer"               "DEL"   #'+jg-ibuffer-default
      :desc "Switch buffer"         ","     #'+jg-ibuffer-ivy-buffer
      )

(setq jg-ibuffer-mode-map (make-keymap))

(define-prefix-command 'jg-ibuffer-filter-map nil "ibuffer-filter")
(define-prefix-command 'jg-ibuffer-sort-map   nil "ibuffer-sort")
(define-prefix-command 'jg-ibuffer-mark-map   nil "ibuffer-mark")

(map! :map jg-ibuffer-mode-map
      :desc "Visit"               :n "RET" #'+ibuffer/visit-workspace-buffer

      :desc "toggle-sorting-mode" :n "," #'ibuffer-toggle-sorting-mode
      :desc "Formats"             :n "." #'ibuffer-switch-format
      :desc "Print Help"          :n "?" #'+jg-ibuffer-print-help

      :desc "Do Shell Cmd File"   :n "!" #'ibuffer-do-shell-command-file

      :desc "add-to-tmp-show"     :n "±" #'ibuffer-add-to-tmp-show
      :desc "add-to-tmp-hide"     :n "§" #'ibuffer-add-to-tmp-hide
      :desc "negative"            :n "_" #'negative-argument

      :desc "Forward Line"        :n "j" #'ibuffer-forward-line
      :desc "Back Line"           :n "k" #'ibuffer-backward-line
      :desc "Mark"                :n "m" #'ibuffer-mark-forward
      :desc "Unmark"              :n "u" #'ibuffer-unmark-forward
      :desc "Unmark All"          :n "U" #'ibuffer-unmark-all-marks
      :desc "Toggle Marks"        :n "t" #'ibuffer-toggle-marks
      :desc "Update"              :n "g" #'ibuffer-update
      :desc "Kill Marked"         :n "D" #'ibuffer-do-delete
      :desc "change-marks"        :n "c" #'ibuffer-change-marks

      :desc "Write Marked"        :n "W" #'ibuffer-do-save

      :n "l" #'ignore
      :n "i" #'ignore
      ;; :n "s" #'ignore
      :n "q" #'kill-current-buffer
      :n "s" #'ibuffer-jump-to-filter-group
      (:prefix ("-" . "Mark Ops"))
      (:prefix ("=" . "Mark All Ops"))
      )

;;-- sorting
(map! :map jg-ibuffer-sort-map
      :desc "Name"             "n"  #'ibuffer-do-sort-by-alphabetic
      :desc "filename/process" "f"  #'ibuffer-do-sort-by-filename/process
      :desc "invert"           "i"  #'ibuffer-invert-sorting
      :desc "major-mode"       "m"  #'ibuffer-do-sort-by-major-mode
      :desc "size"             "s"  #'ibuffer-do-sort-by-size
      :desc "recency"          "v"  #'ibuffer-do-sort-by-recency
      :desc "git status"       "g"  #'ibuffer-do-sort-by-vc-status
      :desc "project"          "p"  #'ibuffer-do-sort-by-project-name
      )

;;-- end sorting

;;-- filtering
(map! :map jg-ibuffer-filter-map ;; filters
      :desc "switch-to-saved-filter-groups"  "."   #'ibuffer-switch-to-saved-filter-groups
      :desc "add-saved-filters"              ","   #'ibuffer-add-saved-filters
      :desc "clear-filter-groups"            "\\"  #'ibuffer-clear-filter-groups
      :desc "pop-filter"                     "p"   #'ibuffer-pop-filter
      :desc "negate-filter"                  "!"   #'ibuffer-negate-filter

      (:prefix ("f" . "filter ops")
       :desc "Choose Filter"                  ","  #'ibuffer-filter-chosen-by-completion
       :desc "Swap filters"                  "TAB" #'ibuffer-exchange-filters
       :desc "and-filter"                    "&"   #'ibuffer-and-filter
       :desc "or-filter"                     "|"   #'ibuffer-or-filter
       :desc "stars"                         "*"   #'ibuffer-filter-by-starred-name
       :desc "disable filter"                "\\"  #'ibuffer-filter-disable
       :desc "decompose-filter"              "d"   #'ibuffer-decompose-filter
       )

      (:prefix ("b" . "by")
       :desc "basename"                      "n"   #'ibuffer-filter-by-basename
       :desc "content"                       "C"   #'ibuffer-filter-by-content
       :desc "derived-mode"                  "M"   #'ibuffer-filter-by-derived-mode
       :desc "directory"                     "d"   #'ibuffer-filter-by-directory
       :desc "filename"                      "f"   #'ibuffer-filter-by-filename
       :desc "mode"                          "m"   #'ibuffer-filter-by-mode
       :desc "modified"                      "c"   #'ibuffer-filter-by-modified
       :desc "name"                          "N"   #'ibuffer-filter-by-name
       :desc "process"                       "p"   #'ibuffer-filter-by-process
       :desc "used-mode"                     "m"   #'ibuffer-filter-by-used-mode
       :desc "visiting-file"                 "v"   #'ibuffer-filter-by-visiting-file
       :desc "by ext"                        "e"   #'ibuffer-filter-by-file-extension
       :desc "by-size-lt"                    "<"   #'ibuffer-filter-by-size-lt
       :desc "by-size-gt"                    ">"   #'ibuffer-filter-by-size-gt
       :desc "filter"                        "'"   #'ibuffer-filter-chosen-by-completion
       :desc "unsaved"                       "u"   #'ibuffer-filter-by-unsaved-buffers
       )
      )

(map! :map jg-ibuffer-filter-map ;; groups
      :prefix ("g" . "Groups")
      :desc "filters-to-filter-group"         "g" #'ibuffer-filters-to-filter-group
      :desc "decompose-filter-group"          "d" #'ibuffer-decompose-filter-group
      :desc "pop-filter-group"                "P" #'ibuffer-pop-filter-group
      :desc "add group"                       "a" #'+jg-ibuffer-add-group
      :desc "sort groups"                     "s" #'+jg-ibuffer-sort-groups
      :desc "Projects"                        "p" (ibuffer-generate! (+jg-ibuffer-generate-project-groups))
      )

(map! :map jg-ibuffer-filter-map ;; save/load
      :prefix ("w" . "Save/Load")
      :desc "delete-saved-filter-groups"     "X" #'ibuffer-delete-saved-filter-groups
      :desc "delete-saved-filters"           "x" #'ibuffer-delete-saved-filters
      :desc "save-filter-groups"             "S" #'ibuffer-save-filter-groups
      :desc "save-filters"                   "s" #'ibuffer-save-filters
      :desc "switch-to-saved-filters"        "r" #'ibuffer-switch-to-saved-filters
      )
;;-- end filtering

;;-- marking
(map! :map jg-ibuffer-mode-map ;; basic
      :prefix ("-" . "Mark Ops")
      :desc "by locked"                  "L" #'ibuffer-mark-by-locked
      :desc "by file name regexp"        "f" #'ibuffer-mark-by-file-name-regexp
      :desc "by content regexp"          "g" #'ibuffer-mark-by-content-regexp
      :desc "by mode regexp"             "m" #'ibuffer-mark-by-mode-regexp
      :desc "by name regexp"             "n" #'ibuffer-mark-by-name-regexp
      )
(map! :map jg-ibuffer-mode-map ;; all
      :prefix ("=" . "Mark All Ops")
      :desc "unmark all"                "*"  #'ibuffer-unmark-all
      :desc "dired buffers"             "\\" #'ibuffer-mark-dired-buffers
      :desc "by mode"                   "M"  #'ibuffer-mark-by-mode
      :desc "dissociated buffers"       "e"  #'ibuffer-mark-dissociated-buffers
      :desc "help buffers"              "h"  #'ibuffer-mark-help-buffers
      :desc "modified buffers"          "m"  #'ibuffer-mark-modified-buffers
      :desc "read only buffers"         "r"  #'ibuffer-mark-read-only-buffers
      :desc "special buffers"           "s"  #'ibuffer-mark-special-buffers
      :desc "unsaved buffers"           "u"  #'ibuffer-mark-unsaved-buffers
      :desc "compressed-file-buffers"   "z"  #'ibuffer-mark-compressed-file-buffers
      :desc "mark-old-buffers"          "o"  #'ibuffer-mark-old-buffers
      )
;;-- end marking

(map! :map jg-ibuffer-mode-map
      "\\" 'jg-ibuffer-filter-map
      "S"  'jg-ibuffer-sort-map
      "s"  'jg-ibuffer-sort-map
      )
(evil-make-overriding-map jg-ibuffer-mode-map 'normal)
(setq ibuffer-mode-map jg-ibuffer-mode-map)
