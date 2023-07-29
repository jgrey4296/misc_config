;;; +jump-map.el -*- lexical-binding: t; -*-

(map! :map jg-binding-jump-map ;; avy
      ;; 1 2 3 "u" "h"
      :desc "Ivy resume"            "`"   #'ivy-resume
      :desc "helms"                 "a" 'jg-binding-helm-map
      :desc "Ibuffer"               "DEL" #'ibuffer
      :desc "Jump to bookmark"      "RET" #'bookmark-jump
      :desc "Last Change"           ";"   #'goto-last-change
      :desc "Jump to Char"          "."   #'avy-goto-char
      :desc "Line"                  "l"   #'evil-avy-goto-line
      :desc "Middle of Line"        "M"   #'evil-middle-of-visual-line
      :desc "imenu"                 "'"   #'counsel-imenu

      :desc "Jump to Window"        "w"   #'ace-window
      :desc "Jump to mark"          "m"   #'evil-show-marks
      :desc "Pop Mark"              "b"   #'avy-pop-mark
      :desc "Push Mark"             "B" (cmd! (avy-push-mark))
      :desc "Goto First Line"       "f"   #'evil-goto-first-line
      :desc "Goto Last Line"        "F"   #'evil-goto-line
      :desc "Search buffer"         "s"   #'swiper
      )
(map! :map jg-binding-jump-map ;; gtags
      :prefix ("g" . "gtags")
      )
(map! :map jg-binding-jump-map ;; search
      :prefix ("/" . "Search")
      :desc "Search Clear"                 "c" #'evil-ex-nohighlight
      :desc "Find File at point"   "F"         #'evil-find-file-at-point-with-line
      :desc "Goto Definition"      "d"         #'evil-goto-definition
      :desc "Lookup File"          "f"         #'+lookup/file
      :desc "Search current directory"     "d" #'+default/search-cwd
      :desc "Lookup"               "D"         #'+lookup/references
      :desc "Next Visual"          "j"         #'evil-next-visual-line
      :desc "Previous Visual"      "k"         #'evil-previous-visual-line
      :desc "Search Word Forward"  "*"         #'evil-ex-search-unbounded-word-forward
      )
