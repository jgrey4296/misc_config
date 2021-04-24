;;; emacs/bindings/+ibuffer-bindings.el -*- lexical-binding: t; -*-

(map! :map ibuffer-mode-map
      :desc "Do Shell Cmd File"   "!" #'ibuffer-do-shell-command-file
      :desc "add-to-tmp-show"     "+" #'ibuffer-add-to-tmp-show
      :desc "toggle-sorting-mode" "," #'ibuffer-toggle-sorting-mode
      :desc "add-to-tmp-hide"     "-" #'ibuffer-add-to-tmp-hide
      :desc "negative"            "-" #'negative-argument
      :desc "mark-old-buffers"    "." #'ibuffer-mark-old-buffers
      :desc "Filters"             "\\" ibuffer--filter-map
)
(map! :map ibuffer-mode-map
      :prefix ("%" . "Mark Ops")
       :desc "mark-by-locked"           "L" #'ibuffer-mark-by-locked
       :desc "mark-by-file-name-regexp" "f" #'ibuffer-mark-by-file-name-regexp
       :desc "mark-by-content-regexp"   "g" #'ibuffer-mark-by-content-regexp
       :desc "mark-by-mode-regexp"      "m" #'ibuffer-mark-by-mode-regexp
       :desc "mark-by-name-regexp"      "n" #'ibuffer-mark-by-name-regexp
       )
(map! :map ibuffer-mode-map
      :prefix ("*" . "Mark All Ops")
       :desc "unmark-all"                   "*" #'ibuffer-unmark-all
       :desc "mark-dired-buffers"           "/" #'ibuffer-mark-dired-buffers
       :desc "mark-by-mode"                 "M" #'ibuffer-mark-by-mode
       :desc "change-marks"                 "c" #'ibuffer-change-marks
       :desc "mark-dissociated-buffers"     "e" #'ibuffer-mark-dissociated-buffers
       :desc "mark-help-buffers"            "h" #'ibuffer-mark-help-buffers
       :desc "mark-modified-buffers"        "m" #'ibuffer-mark-modified-buffers
       :desc "mark-read-only-buffers"       "r" #'ibuffer-mark-read-only-buffers
       :desc "mark-special-buffers"         "s" #'ibuffer-mark-special-buffers
       :desc "mark-unsaved-buffers"         "u" #'ibuffer-mark-unsaved-buffers
       :desc "mark-compressed-file-buffers" "z" #'ibuffer-mark-compressed-file-buffers
       )
(map! :map ibuffer--filter-map
       "<S-up>"                    nil ;;#'ibuffer-pop-filter-group
       "<up>"                      #'ibuffer-pop-filter
       "RET"                       #'ibuffer-filter-by-mode
       "SPC"                       #'ibuffer-filter-chosen-by-completion
       "TAB"                       #'ibuffer-exchange-filters

       :desc "negate-filter"                 "!" #'ibuffer-negate-filter
       :desc "and-filter"                    "&" #'ibuffer-and-filter
       :desc "filter stars"        "*" #'ibuffer-filter-by-starred-name
       :desc "filter file ext"      "." #'ibuffer-filter-by-file-extension
       :desc "filter-disable"                "/" #'ibuffer-filter-disable
       :desc "filter-by-size-lt"             "<" #'ibuffer-filter-by-size-lt
       :desc "filter-by-size-gt"             ">" #'ibuffer-filter-by-size-gt

       :desc "decompose-filter-group"        "D" #'ibuffer-decompose-filter-group
       :desc "filter-by-process"             "E" #'ibuffer-filter-by-process
       :desc "filter-by-directory"           "F" #'ibuffer-filter-by-directory
       :desc "filter-by-derived-mode"        "M" #'ibuffer-filter-by-derived-mode
       :desc "pop-filter-group"              "P" #'ibuffer-pop-filter-group
       :desc "switch-to-saved-filter-groups" "R" #'ibuffer-switch-to-saved-filter-groups

       :desc "save-filter-groups"            "S" #'ibuffer-save-filter-groups
       :desc "delete-saved-filter-groups"    "X" #'ibuffer-delete-saved-filter-groups
       :desc "clear-filter-groups"           "\\" #'ibuffer-clear-filter-groups
       :desc "add-saved-filters"             "a" #'ibuffer-add-saved-filters
       :desc "filter-by-basename"            "b" #'ibuffer-filter-by-basename
       :desc "filter-by-content"             "c" #'ibuffer-filter-by-content
       :desc "decompose-filter"              "d" #'ibuffer-decompose-filter
       :desc "filter-by-predicate"           "e" #'ibuffer-filter-by-predicate
       :desc "filter-by-filename"            "f" #'ibuffer-filter-by-filename
       :desc "filters-to-filter-group"       "g" #'ibuffer-filters-to-filter-group
       :desc "filter-by-modified"            "i" #'ibuffer-filter-by-modified
       :desc "filter-by-used-mode"           "m" #'ibuffer-filter-by-used-mode
       :desc "filter-by-name"                "n" #'ibuffer-filter-by-name
       :desc "or-filter"                     "o" #'ibuffer-or-filter
       :desc "pop-filter"                    "p" #'ibuffer-pop-filter
       :desc "switch-to-saved-filters"       "r" #'ibuffer-switch-to-saved-filters
       :desc "save-filters"                  "s" #'ibuffer-save-filters
       :desc "exchange-filters"              "t" #'ibuffer-exchange-filters
       :desc "filter-by-visiting-file"       "v" #'ibuffer-filter-by-visiting-file
       :desc "delete-saved-filters"          "x" #'ibuffer-delete-saved-filters
       :desc "or-filter"                     "|" #'ibuffer-or-filter
       )

(+jg-binding-keymap-update-plural  ibuffer-mode-map
                                   ibuffer--filter-map)
