;;; +evil-motion-bindings.el -*- lexical-binding: t; -*-

(suppress-keymap jg-binding-motion-state-map)

(map! :map jg-binding-backward-general-motion-map
      :desc "Buffer"       "b"   #'previous-buffer
      :desc "Git Hunk"     "d"   #'git-gutter:previous-hunk
      :desc "Error"        "e"   #'previous-error
      :desc "Heading"      "h"   #'outline-previous-visible-heading
      :desc "Begin Method" "m"   #'+evil/previous-beginning-of-method
      :desc "End Method"   "M"   #'+evil/previous-end-of-method
      :desc "Todo"         "t"   #'hl-todo-previous
      :desc "Workspace"    "w"   #'+workspace/switch-left
      )
(map! :map jg-binding-forward-general-motion-map
      :desc "Section"      "]" #'evil-forward-section-begin
      :desc "Arg"          "a" #'evil-forward-arg
      :desc "Git Hunk"     "d" #'git-gutter:next-hunk
      :desc "Heading"      "h" #'outline-next-visible-heading
      :desc "Begin Method" "m" #'+evil/next-beginning-of-method
      :desc "End Method"   "M" #'+evil/next-end-of-method
      :desc "Section"      "s" #'evil-forward-section-begin
      :desc "Todo"         "t" #'hl-todo-next
      :desc "Workspace"    "w" #'+workspace/switch-right
      :desc "Buffer"       "b" #'next-buffer
      :desc "Comment"      "c" #'+evil/next-comment
      :desc "Error"        "e" #'next-error
      )

(map! :map jg-binding-motion-state-map ;; basic
      :desc "backward-char"         "h"       #'evil-backward-char
      :desc "next-line"             "j"       #'evil-next-line
      :desc "previous-line"         "k"       #'evil-previous-line
      :desc "forward-char"          "l"       #'evil-forward-char

      :desc "Goto Column"            "\\"       #'evil-goto-column
      :desc "Return"                 "RET"     #'evil-ret
      :desc "Search"                 "/"       #'evil-ex-search-forward
      )

(map! :map jg-binding-motion-state-map ;; word, sen, para, line
      ;; Char
      :desc "find-char"             "f"       #'evil-find-char
      ;; :desc "find-char-to"          "t"       #'evil-find-char-to
      :desc "find-char-backward"    "F"       #'evil-find-char-backward
      ;; :desc "find-char-to-backward" "T"       #'evil-find-char-to-backward
      ;; Word
      ;;
      :desc "B-Search Word"         "#"       #'evil-ex-search-word-backward
      :desc "Search Word Forward"   "*"       #'evil-ex-search-word-forward
      :desc "backward-WORD-begin"   "B"       #'evil-backward-WORD-begin
      :desc "backward-word-begin"   "b"       #'evil-backward-word-begin
      :desc "forward-WORD-begin"    "W"       #'evil-forward-WORD-begin
      :desc "forward-word-begin"    "w"       #'evil-forward-word-begin
      :desc "forward-WORD-end"      "E"       #'evil-forward-WORD-end
      :desc "forward-word-end"      "e"       #'evil-forward-word-end
      ;; Para
      :desc "Back Paragraph"        "{"       #'evil-backward-paragraph
      :desc "Forward Paragraph"     "}"       #'evil-forward-paragraph
      ;; Sen
      :desc "F-Sentence"            ")"       #'evil-forward-sentence-begin
      :desc "B-Sentence"            "("       #'evil-backward-sentence-begin
      ;; Line
      :desc "1st Non Blank"         "^"       #'evil-first-non-blank
      :desc "Prev 1st Non Blank"    "-"       #'evil-previous-line-first-non-blank
      :desc "Next 1st Non Blank"    "+"       #'evil-next-line-first-non-blank
      :desc "Next 1st Non Blank"    "_"       #'evil-next-line-1-first-non-blank
      ;; Window
      :desc "window-bottom"         "L"       #'evil-window-bottom
      :desc "window-middle"         "M"       #'evil-window-middle
      :desc "window-top"            "H"       #'evil-window-top
      )

(map! :map jg-binding-motion-state-map
      :desc "Escape"                 "Q"       #'doom/escape
      :desc "Goto Mark"              "`"       #'evil-goto-mark
      :desc "Jump Item"              "%"       #'evil-jump-item
      :desc "Repeat B-Find Char"     ","       #'evil-repeat-find-char-reverse
      :desc "Repeat Find Char"       ";"       #'evil-repeat-find-char
      :desc "Repeat"                 "."       #'evil-repeat
      :desc "Repeat Substitute"      "&"       #'evil-ex-repeat-substitute
      :desc "Scroll Down"            "<down>"  #'evil-scroll-page-down
      :desc "Scroll Left"            "<left>"  #'evil-scroll-left
      :desc "Scroll Right"           "<right>" #'evil-scroll-right
      :desc "Scroll Up"              "<up>"    #'evil-scroll-page-up
      :desc "ex-search-next"         "n"       #'evil-ex-search-next
      :desc "ex-search-previous"     "N"       #'evil-ex-search-previous

      "1"                                      #'digit-argument
      "2"                                      #'digit-argument
      "3"                                      #'digit-argument
      "4"                                      #'digit-argument
      "5"                                      #'digit-argument
      "6"                                      #'digit-argument
      "7"                                      #'digit-argument
      "8"                                      #'digit-argument
      "9"                                      #'digit-argument

      )