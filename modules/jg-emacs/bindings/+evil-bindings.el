;;; emacs/bindings/+evil-maps.el -*- lexical-binding: t; -*-
;; Reminder: evil-mode-map-alist
;;
;; TODO add count lines/words, insert line above/below...

(message "Setting up Evil Bindings: %s" (current-time-string))
;;-- setup
(setq jg-binding-insert-state-map (copy-keymap evil-insert-state-map))

(defvar old-evil-normal-state-map   nil "the original evil-normal-state-map")
(defvar old-evil-iinsert-state-map  nil "the original evil-insert-state-map")
(defvar old-evil-visual-state-map   nil "the original evil-visual-state-map")
(defvar old-evil-operator-state-map nil "the original evil-operator-state-map")
(defvar old-evil-motion-state-map   nil "the original evil-motion-state-map")
;;-- end setup

;;-- normal state
(map! :map jg-binding-normal-state-map ;; State Changes
      :desc "Emacs State"         "C-z"          #'evil-emacs-state
      :desc "Record Macro"        "q"            #'evil-record-macro
      :desc "Force Normal State"  "<escape>"     #'evil-force-normal-state
      :desc "Insert"              "<insert>"     #'evil-insert
      :desc "Insert"              "<insertchar>" #'evil-insert
      :desc "Visual"              "v"            #'evil-visual-char
      (:prefix ("V" . "Visual")
       :desc "line"         "l"            #'evil-visual-line
       :desc "Block"        "b"            #'evil-visual-block
      )

      :desc "Insert"        "i"   #'evil-insert
      :desc "Insert Below"  "o"   #'evil-open-below
      (:prefix ("I" . "Insert Plus")
       :desc "Replace-State" "r"   #'evil-replace-state
       :desc "Append Line"   "a"   #'evil-append-line
       :desc "Prepend Line"  "p"   #'evil-insert-line
       :desc "Sub Line"      "s"   #'evil-change-whole-line
       :desc "Open Above"    "k"   #'evil-open-above
       :desc "Open Below"    "j"   #'evil-open-below
       :desc "Insert after"  "l"   #'evil-append
       )
)
(map! :map jg-binding-normal-state-map ;; Commands
      :desc "Use Register"       "\""  #'evil-use-register
      :desc "Lookup"             "K"   #'+lookup/documentation
      :desc "Indent"             "TAB" #'indent-for-tab-command
      :desc "Macro"              "@"   #'evil-execute-macro
      :desc "Evil-Ex"            ":"   #'evil-ex
      :desc "Invert Char"        "~"   #'evil-invert-char
      :desc "Delete-op"          "d"   #'evil-delete
      :desc "Delete-char"        "x"   #'evil-delete-char
      :desc "B-delete"           "X"  #'evil-delete-backward-char
      :desc "Paste After"        "p"   #'evil-paste-after
      :desc "Redisplay"          "§"   (cmd! (redisplay t))
      :desc "Set Marker"         "m"   #'evil-set-marker
      :desc "Replace"            "r"   #'evil-replace

      :desc "Undo"               "u"   #'evil-undo
      :desc "Yank"               "y"   #'evil-yank
      :desc "Yank-line"          "Y"   #'evil-yank-line

      (:prefix ("D" . "Deletion")
       :desc "B-delete"           "H"   #'evil-delete-backward-char
       :desc "B-delete word"      "h"   #'evil-delete-backward-word
       :desc "Delete"             "l"   #'evil-delete-char
       :desc "Delete Line"        "L"   #'evil-delete-line
      )
      (:prefix ("C" . "Change")
       :desc "Replace"           "r"   #'evil-replace
       :desc "Indent"            "i"   #'evil-indent
       :desc "Change-op"         "c"   #'evil-change
       :desc "Invert Char"       "v"   #'evil-invert-char
       :desc "Join line"         "j"   #'evil-join
       :desc "Shift Left"        "l"   #'evil-shift-left
       :desc "Shift Right"       "h"   #'evil-shift-right
       )
      (:prefix ("P" . "Paste")
       :desc "Paste After"        "l"   #'evil-paste-after
       :desc "Paste Before"       "h"   #'evil-paste-before
       )

      )
(map! :map jg-binding-normal-state-map ;; chords
      :desc "Repeat Pop"        "C-." #'evil-repeat-pop
      :desc "Paste Pop Next"    "C-n" #'evil-paste-pop-next
      :desc "Paste Pop"         "C-p" #'evil-paste-pop
      :desc "Redo"              "C-r" #'evil-redo
      :desc "Pop Tag Mark"      "C-t" #'pop-tag-mark
      :desc "Repeat Pop Next"   "M-." #'evil-repeat-pop-next
      :desc "Paste Pop"         "M-y" #'evil-paste-pop

      "C-f" #'evil-scroll-page-down
      "C-b" #'evil-scroll-page-up

      :desc "Delete" "<deletechar>"         #'evil-delete-char
      :desc "Back Char" "DEL"               #'evil-backward-char

      "c" #'ignore
      "X" #'ignore
      "O" #'ignore
      )
;;-- end normal state

;;-- insert state
(map! :map jg-binding-insert-state-map
      [escape] 'evil-normal-state
      :desc "Escape"        "C-g" #'evil-escape
      ;; TAB
      )
;;-- end insert state

;;-- visual state
(map! :map jg-binding-visual-state-map
      [escape] 'evil-normal-state
      "V" nil
      :desc "Line select"             "L"        #'evil-visual-line
      :desc "Block Select"            "B"        #'evil-visual-block
      :desc "B-Search"                "\\"       #'evil-ex-search-backward
      :desc "Change"                  "R"        #'evil-change
      :desc "Escape"                  "C-g"      #'evil-escape
      :desc "Exchange Corners"        "O"        #'evil-visual-exchange-corners
      :desc "Exchange Point and Mark" "o"        #'exchange-point-and-mark
      :desc "Exit"                    "<escape>" #'evil-exit-visual-state
      :desc "Exit"                    "v"        #'evil-exit-visual-state
      :desc "Indent"                  "TAB"      #'indent-for-tab-command
      :desc "L-Shift"                 "<"        #'+evil/shift-left
      :desc "Macro"                   "@"        #'+evil:apply-macro
      :desc "R-Shift"                 ">"        #'+evil/shift-right
      :desc "Search"                  "/"        #'evil-ex-search-forward
      :desc "Surround"                "s"        #'evil-surround-region
      :desc "Visual Search"           "?"        #'evil-visualstar/begin-search-forward
      :desc "Yank"                    "y"        #'evil-yank
      )
;;-- end visual state

;;-- operator state
(map! :map jg-binding-operator-state-map
      [escape] 'evil-normal-state
      :desc "Escape"        "C-g" #'evil-escape
      ;; l
      ;; RET
      )
;;-- end operator state

;;-- motion state
(map! :map jg-binding-motion-state-map ;; basic
      :desc "backward-char"         "h"       #'evil-backward-char
      :desc "next-line"             "j"       #'evil-next-line
      :desc "previous-line"         "k"       #'evil-previous-line
      :desc "forward-char"          "l"       #'evil-forward-char

      :desc "EOL"                    "$"       #'evil-end-of-line
      :desc "BOL"                    "0"       #'evil-beginning-of-line
      :desc "Return"                 "RET"     #'evil-ret
      :desc "Search"                 "/"       #'evil-ex-search-forward
      :desc "B-Search"               "\\"      #'evil-ex-search-backward
      )

(map! :map jg-binding-motion-state-map ;; word, sen, para, line
      ;; Char
      :desc "find-char"             "f"       #'evil-find-char
      :desc "find-char-to"          "t"       #'evil-find-char-to
      :desc "find-char-to-backward" "T"       #'evil-find-char-to-backward
      ;; Word
      :desc "B-Search Word"         "#"       #'evil-ex-search-word-backward
      :desc "Search Word Forward"   "*"       #'evil-ex-search-word-forward
      :desc "backward-WORD-begin"   "B"       #'evil-backward-WORD-begin
      :desc "backward-word-begin"   "b"       #'evil-backward-word-begin
      :desc "find-char-backward"    "F"       #'evil-find-char-backward
      :desc "forward-WORD-begin"    "W"       #'evil-forward-WORD-begin
      :desc "forward-WORD-end"      "E"       #'evil-forward-WORD-end
      :desc "forward-word-begin"    "w"       #'evil-forward-word-begin
      :desc "forward-word-end"      "e"       #'evil-forward-word-end
      ;; Para
      :desc "Back Paragraph"        "{"       #'evil-backward-paragraph
      :desc "Forward Paragraph"     "}"       #'evil-forward-paragraph
      ;; Sen
      :desc "F-Sentence"            ")"       #'evil-forward-sentence-begin
      :desc "B-Sentence"            "("       #'evil-backward-sentence-begin
      ;; Line
      :desc "1st Non Blank"         "^"       #'evil-first-non-blank
      :desc "Goto Mark Line"        "'"       #'evil-goto-mark-line
      :desc "Prev 1st Non Blank"    "-"       #'evil-previous-line-first-non-blank
      :desc "Next 1st Non Blank"    "+"       #'evil-next-line-first-non-blank
      :desc "Next 1st Non Blank"    "_"       #'evil-next-line-1-first-non-blank
      :desc "goto-line"             "G"       #'evil-goto-line
      ;; Window
      :desc "window-bottom"         "L"       #'evil-window-bottom
      :desc "window-middle"         "M"       #'evil-window-middle
      :desc "window-top"            "H"       #'evil-window-top
      )

(map! :map jg-binding-motion-state-map
      :desc "Goto Column"            "|"       #'evil-goto-column
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
;;-- end motion state

;;-- operators
(map! :map jg-binding-operator-map
      ;; g > s
      :desc "Repeat Global Sub"   "&"   #'evil-ex-repeat-global-substitute

      :desc "Incr"                "="   #'evil-numbers/inc-at-pt
      :desc "Incr"                "+"   #'evil-numbers/inc-at-pt
      :desc "Decr"                "-"   #'evil-numbers/dec-at-pt
      :desc "Last Change"         ";"   #'goto-last-change
      :desc "Apply Macro"         "@"   #'+evil:apply-macro
      :desc "Line End"            "$"   #'evil-end-of-visual-line
      :desc "Line Start"          "0"   #'evil-beginning-of-visual-line
      :desc "Char"                "?"   #'what-cursor-position
      :desc "Jump Char"           "."   #'evil-avy-goto-char
      :desc "Jump to Tag"         ","   #'helm-gtags-find-tag

      :desc "Invert"              "~"   #'evil-invert-case

      :desc "Align"              "a" #'align-regexp
      :desc "Pop Mark"           "b" #'avy-pop-mark
      :desc "Push Mark"          "B" #'avy-push-mark
      :desc "Comment"            "c" #'evilnc-comment-operator
      :desc "IEdit"              "e" #'iedit-mode
      :desc "Goto First Line"    "f" #'evil-goto-first-line
      :desc "Insert Resume"      "i" #'evil-insert-resume
      :desc "Join whitespace"    "J" #'evil-join-whitespace

      :desc "Middle of Line"     "m" #'evil-middle-of-visual-line
      :desc "Insert Line"        "o" #'+evil/insert-newline-below

      :desc "Replace region"     "R" #'+eval:replace-region

      :desc "Upper"              "U"   #'evil-upcase
      :desc "Down"               "u"   #'evil-downcase

      :desc "Restore selection"  "v"   #'evil-visual-restore
      :desc "Wrap Line"          "w"   #'evil-fill
      :desc "Fill"               "q"   #'evil-fill-and-move
      :desc "Exchange"           "x"   #'evil-exchange
      :desc "Yank"               "y"   #'+evil:yank-unindented
      :desc "Zap to Char"        "z"   #'zap-up-to-char

      (:prefix ("s" . "String-ops")
       ;; t
       :desc "Rot13"               "'" #'evil-rot13
       :desc "Decode url"         "E"  #'+evil:url-decode
       :desc "Encode url"         "e"  #'+evil:url-encode
       :desc "Inflection"         "i"  #'evil-operator-string-inflection
       :desc "ispell-word"        "="   #'ispell-word
       )
      (:prefix ("/" . "Search")
       :desc "Search Word Forward" "*"  #'evil-ex-search-unbounded-word-forward
       :desc "Goto Definition"      "d" #'evil-goto-definition
       :desc "Lookup"               "D" #'+lookup/references
       :desc "Find File at point"   "F" #'evil-find-file-at-point-with-line
       :desc "Lookup File"          "f" #'+lookup/file
       :desc "Next Visual"          "j" #'evil-next-visual-line
       :desc "Previous Visual"      "k" #'evil-previous-visual-line
       :desc "Regexp Builder"       "r" #'regexp-builder
       ;; TODO escalate replace op
       )

      (:prefix ("l" . "Line-ops")
       ;; TODO uniquify, remove leading whitespace, split on char
       ;; r
       :desc "Justify" "j"                    #'justify-current-line
       :desc "Flush Lines"                "f" #'flush-lines
       :desc "Keep Lines"                 "K" #'keep-lines
       :desc "Delete trailing newlines"   "W" #'doom/delete-trailing-newlines
       :desc "Delete trailing whitespace" "w" #'delete-trailing-whitespace
       :desc "Whitespace Cleanup"         "c" #'whitespace-cleanup
       :desc "Indent"                     "i" #'indent-region
       )
      )
;;-- end operators

;;-- text objects
(map! :map jg-binding-inner-text-objects-map
      :desc "\""                   "\""  #'evil-inner-double-quote
      :desc "'"                    "'"   #'evil-inner-single-quote
      :desc "`'"                   "`"   #'evil-inner-back-quote
      :desc "("                    "("   #'evil-inner-paren
      :desc "<"                    "<"   #'evil-inner-angle
      :desc "["                    "["   #'evil-inner-bracket
      :desc "{"                    "{"   #'evil-inner-curly

      :desc "arg"                  "a"   #'evil-inner-arg
      :desc "paren"                "b"   #'evil-inner-paren
      :desc "block"                "B"   #'evil-textobj-anyblock-inner-block
      :desc "comment"              "c"   #'evilnc-inner-comment
      :desc "defun"                "f"   #'+evil:defun-txtobj
      :desc "whole-buffer"         "g"   #'+evil:whole-buffer-txtobj
      :desc "Same Indent Block"    "i"   #'evil-indent-plus-i-indent
      :desc "+Same Indent Block+"  "j"   #'evil-indent-plus-i-indent-up-down
      :desc "+Same Indent Block"   "k"   #'evil-indent-plus-i-indent-up
      :desc "Symbol"               "o"   #'evil-inner-symbol
      :desc "Paragraph"            "p"   #'evil-inner-paragraph
      :desc "Any-Quote"            "q"   #'+evil:inner-any-quote
      :desc "Sentence"             "s"   #'evil-inner-sentence
      :desc "XML Tag"              "t"   #'evil-inner-tag
      :desc "URL"                  "u"   #'+evil:inner-url-txtobj
      :desc "WORD"                 "W"   #'evil-inner-WORD
      :desc "word"                 "w"   #'evil-inner-word
      :desc "XML Attr"             "x"   #'evil-inner-xml-attr
      )
(map! :map jg-binding-outer-text-objects-map
      :desc  "\""                   "\"" #'evil-a-double-quote
      :desc  "'"                    "'"  #'evil-a-single-quote
      :desc  "`'"                   "`"  #'evil-a-back-quote
      :desc  "("                    "("  #'evil-a-paren
      :desc  "<"                    "<"  #'evil-an-angle
      :desc  "["                    "["  #'evil-a-bracket
      :desc  "{"                    "{"  #'evil-a-curly

      :desc  "arg"                  "a"  #'evil-outer-arg
      :desc  "paren"                "b"  #'evil-a-paren
      :desc  "lock"                 "B"  #'evil-textobj-anyblock-a-block
      :desc  "comment"              "c"  #'evilnc-outer-commenter
      :desc  "defun"                "f"  #'+evil:defun-txtobj
      :desc  "whole-buffer"         "g"  #'+evil:whole-buffer-txtobj
      :desc  "Same Indent Block"    "i"  #'evil-indent-plus-a-indent
      :desc  "+Same Indent Block+"  "j"  #'evil-indent-plus-a-indent-up-down
      :desc  "+Same Indent Block"   "k"  #'evil-indent-plus-a-indent-up
      :desc  "Symbol"               "o"  #'evil-a-symbol
      :desc  "Paragraph"            "p"  #'evil-a-paragraph
      :desc  "Any-Quote"            "q"  #'+evil:outer-any-quote
      :desc  "Sentence"             "s"  #'evil-a-sentence
      :desc  "XML Tag"              "t"  #'evil-a-tag
      :desc  "URL"                  "u"  #'+evil:outer-url-txtobj
      :desc  "WORD"                 "W"  #'evil-a-WORD
      :desc  "word"                 "w"  #'evil-a-word
      :desc  "XML attr"             "x"  #'evil-outer-xml-attr
      )
;;-- end text objects

;;-- operator motion
(map! :map jg-binding-backward-operator-motion-map
      ;; RET ] l r
      :desc "Section"      "["   #'evil-backward-section-begin
      :desc "Arg"          "a"   #'evil-backward-arg
      :desc "Comment"      "c"   #'+evil/previous-comment
      :desc "Git Hunk"     "d"   #'git-gutter:previous-hunk
      :desc "Heading"      "h"   #'outline-previous-visible-heading
      :desc "Begin Method" "m"   #'+evil/previous-beginning-of-method
      :desc "End Method"   "M"   #'+evil/previous-end-of-method
      :desc "Section"      "s"   #'evil-backward-section-begin


      ;; TODO rotate text
      ;;  (:when (featurep! :editor rotate-text)
      ;; :n "]r"  #'rotate-text
      ;; :n "[r"  #'rotate-text-backward)

      )
(map! :map jg-binding-forward-operator-motion-map
      ;; r RET l [
      :desc "Section"      "]" #'evil-forward-section-begin
      :desc "Arg"          "a" #'evil-forward-arg
      :desc "Git Hunk"     "d" #'git-gutter:next-hunk
      :desc "Heading"      "h" #'outline-next-visible-heading
      :desc "Begin Method" "m" #'+evil/next-beginning-of-method
      :desc "End Method"   "M" #'+evil/next-end-of-method
      :desc "Section"      "s" #'evil-forward-section-begin
      )
;;-- end operator motion

;;-- non-text motion
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
;;-- end non-text motion

;;-- vision
(map! :map jg-binding-vision-map
      ;; RET, 1, aAdocrjkIi
      :desc "Widen"         "DEL" #'widen
      :desc "Scroll Right"  ">"   #'evil-scroll-column-right
      :desc "Scroll Left"   "<"   #'evil-scroll-column-left


      :desc "Center" "z"          #'evil-scroll-line-to-center
      :desc "Top"    "t"          #'evil-scroll-line-to-top
      :desc "Bottom" "b"          #'evil-scroll-line-to-bottom

      (:prefix ("'" . "Highlight")
       ;; Reserved
       )

      (:prefix ("v" . "Vimish Fold")
       ;; Reserved
       )
      )
;;-- end vision

;;-- stitching together
(map! :map jg-binding-normal-state-map
      :desc "Do Ops"        "g"   jg-binding-operator-map
      :desc "Visual Ops"    "z"   jg-binding-vision-map
      :desc "B Motion"      "["   jg-binding-backward-general-motion-map
      :desc "F Motion"      "]"   jg-binding-forward-general-motion-map
      )

(map! :map jg-binding-visual-state-map
      :desc "Do Ops"           "g" jg-binding-operator-map
      :desc "Visual Ops"       "z" jg-binding-vision-map
      )

(map! :map jg-binding-motion-state-map
      :desc "Backward Motion Op"  "[" jg-binding-backward-operator-motion-map
      :desc "Forward Motion Op"   "]" jg-binding-forward-operator-motion-map
      :desc "Inner Select"        "i" jg-binding-inner-text-objects-map
      :desc "Outer Select"        "a" jg-binding-outer-text-objects-map
      )

(map! :map jg-binding-operator-state-map
      :desc "Backward Motion Op"  "[" jg-binding-backward-operator-motion-map
      :desc "Forward Motion Op"   "]" jg-binding-forward-operator-motion-map
      :desc "Inner Select"        "i" jg-binding-inner-text-objects-map
      :desc "Outer Select"        "a" jg-binding-outer-text-objects-map
      )

(after! jg-dired-bindings
  ;; Override default evil maps
  (message "Finalising Evil bindings: %s" (current-time-string))
  ;; Backup
  (setq old-evil-normal-state-map evil-normal-state-map
        old-evil-visual-state-map evil-visual-state-map
        old-evil-operator-state-map evil-operator-state-map
        old-evil-motion-state-map evil-motion-state-map
        old-evil-insert-state-map evil-insert-state-map
        )

  ;; Override
  (setq evil-normal-state-map jg-binding-normal-state-map
        evil-insert-state-map jg-binding-insert-state-map
        evil-visual-state-map jg-binding-visual-state-map
        evil-operator-state-map jg-binding-operator-state-map
        evil-motion-state-map jg-binding-motion-state-map
        evil-inner-text-objects-map jg-binding-inner-text-objects-map
        evil-outer-text-objects-map jg-binding-outer-text-objects-map
        )

  ;; Refresh
  (setq evil-global-keymaps-alist
        '((evil-emacs-state-minor-mode    . evil-emacs-state-map)
          (evil-motion-state-minor-mode   . evil-motion-state-map)
          (evil-replace-state-minor-mode  . evil-replace-state-map)
          (evil-operator-state-minor-mode . evil-operator-state-map)
          (evil-visual-state-minor-mode   . evil-visual-state-map)
          (evil-insert-state-minor-mode   . evil-insert-state-map)
          (evil-normal-state-minor-mode   . evil-normal-state-map)))
  (message "Evil Bindings Complete: %s" (current-time-string))

  (global-set-key (kbd "<backtab>")       #'evil-normal-state)

  (provide 'jg-evil-bindings)
  )
;;-- end stitching together