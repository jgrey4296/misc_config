;;; +jump-map.el -*- lexical-binding: t; -*-

(map! :map jge-jump-map ;; groups
      (:prefix ("k" . "Documentation"))
      (:prefix ("g" . "gtags"))
      (:prefix ("/" . "Search"))
      )

(map! :map jge-jump-map ;; avy
      ;; 1 2 3 "u" "h"
      ;; :desc "Ivy resume"            "`"   #'ivy-resume
      "c" #'ignore
      :desc "helms"                 "a"   jge-helm-map
      :desc "Last Change"           ";"   #'goto-last-change
      :desc "Jump to Char"          "."   #'avy-goto-char
      :desc "Line"                  "l"   #'evil-avy-goto-line
      :desc "Middle of Line"        "M"   #'evil-middle-of-visual-line
      :desc "imenu"                 "'"   #'imenu

      ;; w for window switching, in window-nav
      ;; :desc "Jump to Window"        "w"   #'ace-window
      :desc "Jump to mark"          "m"   #'evil-show-marks
      :desc "Jump Back"             "b"   #'avy-pop-mark
      :desc "Jump Next"             "n"   (cmd! (avy-push-mark))
      :desc "Goto First Line"       "["   #'+jg-evil-bob-with-mark
      :desc "Goto Last Line"        "]"   #'evil-goto-line
      :desc "Search buffer"         "s"   #'isearch-forward
      )

(map! :map jge-jump-map ;; search
      :prefix ("/" . "Search")
      :desc "Search Clear"                "c"         #'evil-ex-nohighlight
      :desc "Find File at point"          "F"         #'evil-find-file-at-point-with-line
      :desc "Goto Definition"             "d"         #'evil-goto-definition
      :desc "Next Visual"                 "j"         #'evil-next-visual-line
      :desc "Previous Visual"             "k"         #'evil-previous-visual-line
      :desc "Search Word Forward"         "*"         #'evil-ex-search-unbounded-word-forward
      )
