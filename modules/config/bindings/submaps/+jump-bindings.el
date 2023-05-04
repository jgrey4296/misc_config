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
(map! :map jg-binding-jump-map ;; lookup
      :desc "Type definition"       "t" #'+lookup/type-definition
      :desc "References"            "r" #'+lookup/references
      :desc "Definition"            "d" #'+lookup/definition
      :desc "Implementations"       "i" #'+lookup/implementations
      :desc "Find other file"       "o" #'projectile-toggle-between-implementation-and-test
      ;; :desc "Documentation"      "k" #'+lookup/documentation
      )
(map! :map jg-binding-jump-map ;; gtags
      :prefix ("g" . "gtags")
      :desc "Create Tags"           "c" #'helm-gtags-create-tags
      :desc "Find Symbol"           "y" #'helm-gtags-find-symbol
      :desc "Find Tag Other Window" "o" #'helm-gtags-find-tag-other-window
      :desc "Find Tag"              "d" #'helm-gtags-find-tag
      :desc "Find rtag"             "r" #'helm-gtags-find-rtag
      :desc "Gtags Select"          "s" #'helm-gtags-select
      :desc "Parse File"            "p" #'helm-gtags-parse-file
      :desc "Tags in func"          "i" #'helm-gtags-tags-in-this-function
      :desc "Update Tags"           "u" #'helm-gtags-update-tags
      )
(map! :map jg-binding-jump-map ;; search
      :prefix ("/" . "Search")
      :desc "Find File at point"   "F" #'evil-find-file-at-point-with-line
      :desc "Goto Definition"      "d" #'evil-goto-definition
      :desc "Lookup File"          "f" #'+lookup/file
      :desc "Lookup"               "D" #'+lookup/references
      :desc "Next Visual"          "j" #'evil-next-visual-line
      :desc "Previous Visual"      "k" #'evil-previous-visual-line
      :desc "Search Word Forward"  "*" #'evil-ex-search-unbounded-word-forward
      )
