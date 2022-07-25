;;; emacs/bindings/+evil-maps.el -*- lexical-binding: t; -*-
;; Reminder: evil-mode-map-alist
;;
;; TODO add count lines/words, insert line above/below...

(message "Setting up Evil Bindings: %s" (current-time-string))
(defvar jg-binding-normal-state-map (make-sparse-keymap) "JG map replacing evil-normal-state-map")
(defvar jg-binding-visual-state-map (make-sparse-keymap) "JG map replacing evil-visual-state-map")
(defvar jg-binding-operator-state-map (make-sparse-keymap) "JG map replacing evil-operator-state-map")
(defvar jg-binding-motion-state-map (make-sparse-keymap) "JG map replacing evil-motion-state-map")
(defvar jg-binding-inner-text-objects-map (make-sparse-keymap) "JG map for selectin text objects")
(defvar jg-binding-outer-text-objects-map (make-sparse-keymap) "JG map replacing evil-outer-text-objects-map")
(defvar jg-binding-insert-state-map (copy-keymap evil-insert-state-map))

(defvar old-evil-normal-state-map nil "the original evil-normal-state-map")
(defvar old-evil-iinsert-state-map nil "the original evil-insert-state-map")
(defvar old-evil-visual-state-map nil "the original evil-visual-state-map")
(defvar old-evil-operator-state-map nil "the original evil-operator-state-map")
(defvar old-evil-motion-state-map nil "the original evil-motion-state-map")

;; Normal state
(map! :map jg-binding-normal-state-map
      :desc "Use Register"      "\""  #'evil-use-register
      :desc "Repeat Substitute" "& "  #'evil-ex-repeat-substitute
      :desc "Repeat"            ". "  #'evil-repeat
      :desc "Shift Left"        "< "  #'evil-shift-left
      :desc "Shift Right"       "> "  #'evil-shift-right
      :desc "Invert Char"       "~"   #'evil-invert-char
      :desc "Indent"            "= "  #'evil-indent
      :desc "Macro"             "@ "  #'evil-execute-macro

      :desc "Search"   "/"   #'evil-ex-search-forward
      :desc "B-Search" "\\"  #'evil-ex-search-backward

      :desc "Append"        "a" #'evil-append
      :desc "Change"        "c" #'evil-change
      :desc "Delete"        "d" #'evil-delete
      :desc "Delete Line"   "D" #'evil-delete-line
      :desc "Insert"        "i" #'evil-insert
      :desc "Set Marker"    "m" #'evil-set-marker
      :desc "Open Below"    "o" #'evil-open-below
      :desc "Paste After"   "p" #'evil-paste-after
      :desc "Record Macro"  "q" #'evil-record-macro
      :desc "Replace"       "r" #'evil-replace
      :desc "Substitute"    "s" #'evil-substitute
      :desc "Undo"          "u" #'evil-undo
      :desc "Delete"        "x" #'evil-delete-char
      :desc "Yank"          "y" #'evil-yank

      :desc "Append Line"   "A" #'evil-append-line
      :desc "Delete Line"   "D" #'evil-delete-line
      :desc "Insert Line"   "I" #'evil-insert-line
      :desc "Join"          "J" #'evil-join
      :desc "Lookup"        "K" #'+lookup/documentation
      :desc "Open Above"    "O" #'evil-open-above
      :desc "Paste Before"  "P" #'evil-paste-before
      :desc "Eval Expr"     "Q" #'pp-eval-expression
      :desc "Replace-State" "R" #'evil-replace-state
      :desc "Change Line"   "S" #'evil-change-whole-line
      :desc "B-delete"      "X" #'evil-delete-backward-char
      :desc "Yank Line"     "Y" #'evil-yank-line
      )
(map! :map jg-binding-normal-state-map
      :desc "Repeat Pop"        "C-." #'evil-repeat-pop
      :desc "Paste Pop Next"    "C-n" #'evil-paste-pop-next
      :desc "Paste Pop"         "C-p" #'evil-paste-pop
      :desc "Redo"              "C-r" #'evil-redo
      :desc "Pop Tag Mark"      "C-t" #'pop-tag-mark
      :desc "Repeat Pop Next"   "M-." #'evil-repeat-pop-next
      :desc "Paste Pop"         "M-y" #'evil-paste-pop
      :desc "Emacs State"       "C-z" #'evil-emacs-state

      "C-f" #'evil-scroll-page-down
      "C-b" #'evil-scroll-page-up

      :desc "Delete" "<deletechar>"         #'evil-delete-char
      :desc "Force Normal State" "<escape>" #'evil-force-normal-state
      :desc "Insert" "<insert>"             #'evil-insert
      :desc "Insert" "<insertchar>"         #'evil-insert
      :desc "Back Char" "DEL"               #'evil-backward-char
      )
;; Insert state
(map! :map jg-binding-insert-state-map
      "TAB" #'+jg-completion-complete-or-snippet

      )
;; Visual state
(map! :map jg-binding-visual-state-map
      :desc "Mark Buffer"   "RET"      #'+jg-text-whole-buffer-textobj
      :desc "Visual Block"  "DEL"      #'evil-visual-block
      :desc "Repeat"        "."        #'evil-repeat
      :desc "Search"        "/"        #'evil-ex-search-forward
      :desc "B-Search"     "\\"        #'evil-ex-search-backward
      :desc "Visual Search" "?"        #'evil-visualstar/begin-search-forward
      :desc "L-Shift"       "<"        #'+evil/shift-left
      :desc "R-Shift"       ">"        #'+evil/shift-right
      :desc "Exit"          "<escape>" #'evil-exit-visual-state
      :desc "Escape"        "C-g"      #'evil-escape
      :desc "Macro"         "@"        #'+evil:apply-macro


      :desc "Lookup"                  "K" #'+lookup/documentation
      :desc "Exchange Corners"        "O" #'evil-visual-exchange-corners
      :desc "Change"                  "R" #'evil-change
      :desc "Surround"                "s" #'evil-surround-region
      :desc "Surround Change"         "S" #'evil-surround-change
      :desc "Exchange Point and Mark" "o" #'exchange-point-and-mark
      )
;; Operator state
(map! :map jg-binding-operator-state-map
      :desc "Escape"        "C-g" #'evil-escape
      :desc "Surround Edit" "S"   #'evil-Surround-edit
      :desc "Surround"      "s"   #'evil-surround-edit
      :desc "Select Line"   "l"   #'+jg-text-line-textobj
      :desc "Select Buffer" "RET" #'+jg-text-whole-buffer-textobj
      )
;; Motion state
(map! :map jg-binding-motion-state-map
      :desc "Indent" "TAB"            #'indent-for-tab-command
      :desc "Return" "RET"            #'evil-ret
      :desc "Emacs State"       "C-z" #'evil-emacs-state
      :desc "Redisplay"           "§" (cmd! (redisplay t))


      :desc "1st Non Blank"       "^" #'evil-first-non-blank
      :desc "Next 1st Non Blank " "_" #'evil-next-line-1-first-non-blank
      :desc "Goto Mark"           "`" #'evil-goto-mark
      :desc "Evil-Ex"             ":" #'evil-ex
      :desc "Repeat Find Char"    ";" #'evil-repeat-find-char
      :desc "B-Search"            "?" #'evil-ex-search-backward
      :desc "Back Paragraph"      "{" #'evil-backward-paragraph
      :desc "Goto Column"         "|" #'evil-goto-column
      :desc "Forward Paragraph"   "}" #'evil-forward-paragraph

      :desc "Scroll Left" "<left>"    #'evil-scroll-left
      :desc "Scroll Right" "<right>"  #'evil-scroll-right
      :desc "Scroll Up" "<up>"        #'evil-scroll-page-up
      :desc "Scroll Down" "<down>"    #'evil-scroll-page-down

      :desc "Shell Cmd"           "!" #'evil-shell-command
      :desc "B-Search Word"       "#" #'evil-ex-search-word-backward
      :desc "EOL"                 "$" #'evil-end-of-line
      :desc "Jump Item"           "%" #'evil-jump-item
      :desc "Goto Mark Line"      "'" #'evil-goto-mark-line
      :desc "B-Sentence"          "(" #'evil-backward-sentence-begin
      :desc "F-Sentence"          ")" #'evil-forward-sentence-begin
      :desc "Search Word Forward" "*" #'evil-ex-search-word-forward
      :desc "Next 1st Non Blank"  "+" #'evil-next-line-first-non-blank
      :desc "Repeat B-Find Char"  "," #'evil-repeat-find-char-reverse
      :desc "Prev 1st Non Blank"  "-" #'evil-previous-line-first-non-blank
      :desc "Search"              "/" #'evil-ex-search-forward
      :desc "BOL"                 "0" #'evil-beginning-of-line
      "1"             #'digit-argument
      "2"             #'digit-argument
      "3"             #'digit-argument
      "4"             #'digit-argument
      "5"             #'digit-argument
      "6"             #'digit-argument
      "7"             #'digit-argument
      "8"             #'digit-argument
      "9"             #'digit-argument

      :desc  "backward-WORD-begin"   "B"              #'evil-backward-WORD-begin
      :desc  "forward-WORD-end"      "E"              #'evil-forward-WORD-end
      :desc  "find-char-backward"    "F"              #'evil-find-char-backward
      :desc  "goto-line"             "G"              #'evil-goto-line
      :desc  "window-top"            "H"              #'evil-window-top
      :desc  "lookup"                "K"              #'evil-lookup
      :desc  "window-bottom"         "L"              #'evil-window-bottom
      :desc  "window-middle"         "M"              #'evil-window-middle
      :desc  "ex-search-previous"    "N"              #'evil-ex-search-previous
      :desc  "find-char-to-backward" "T"              #'evil-find-char-to-backward
      :desc  "visual-line"           "V"              #'evil-visual-line
      :desc  "forward-WORD-begin"    "W"              #'evil-forward-WORD-begin
      :desc  "yank-line"             "Y"              #'evil-yank-line

      :desc  "backward-word-begin"   "b"              #'evil-backward-word-begin
      :desc  "forward-word-end"      "e"              #'evil-forward-word-end
      :desc  "find-char"             "f"              #'evil-find-char
      :desc  "backward-char"         "h"              #'evil-backward-char
      :desc  "next-line"             "j"              #'evil-next-line
      :desc  "previous-line"         "k"              #'evil-previous-line
      :desc  "forward-char"          "l"              #'evil-forward-char
      :desc  "ex-search-next"        "n"              #'evil-ex-search-next
      :desc  "find-char-to"          "t"              #'evil-find-char-to
      :desc  "visual-char"           "v"              #'evil-visual-char
      :desc  "forward-word-begin"    "w"              #'evil-forward-word-begin
      :desc  "yank"                  "y"              #'evil-yank

      :desc "visual-block"           "C-v"            #'evil-visual-block
      )
;; Vision / Hiding
(map! :map jg-binding-vision-map
      :desc "Narrow"        "RET" #'+jg-narrow-around-point
      :desc "Widen"         "DEL" #'widen
      :desc "ispell-word"   "="  #'ispell-word
      :desc "Scroll Right"  ">"  #'evil-scroll-column-right
      :desc "Scroll Left"   "<"  #'evil-scroll-column-left

      :desc "open-fold-rec" "A" #'evil-open-fold-rec

      :desc "toggle-fold"   "a" #'evil-toggle-fold
      :desc "close-fold"    "d" #'evil-close-fold
      :desc "Next Fold"     "j" #'+fold/next
      :desc "Prev Fold"     "k" #'+fold/previous
      :desc "open-fold"     "o" #'evil-open-fold
      :desc "open-folds"    "r" #'evil-open-folds
      :desc "Close Folds"   "m" #'evil-close-folds

      :desc "Center" "z"   #'evil-scroll-line-to-center
      :desc "Top"    "t"   #'evil-scroll-line-to-top
      :desc "Bottom" "b"   #'evil-scroll-line-to-bottom

      (:prefix ("'" . "Highlight")
       :desc  "symbol-at-point"            "." #'highlight-symbol-at-point
       :desc  "find-patterns"              "f" #'hi-lock-find-patterns
       :desc  "write-interactive-patterns" "i" #'hi-lock-write-interactive-patterns
       :desc  "lines-matching-regexp"      "l" #'highlight-lines-matching-regexp
       :desc  "phrase"                     "p" #'highlight-phrase
       :desc  "regexp"                     "r" #'highlight-regexp
       :desc  "unhighlight-regexp"         "u" #'unhighlight-regexp
       )

      (:prefix ("v" . "Vimish Fold")
       :desc "toggle-all"             "A"  #'vimish-fold-toggle-all
       :desc "delete-all"             "D"  #'vimish-fold-delete-all
       :desc "toggle"                 "a"  #'vimish-fold-toggle
       :desc "delete"                 "d"  #'vimish-fold-delete
       :desc "fold"                   "f"  #'vimish-fold
       :desc "next-fold"              "j"  #'vimish-fold-next-fold
       :desc "previous-fold"          "k"  #'vimish-fold-previous-fold
       :desc "refold-all"             "m"  #'vimish-fold-refold-all
       :desc "unfold-all"             "r"  #'vimish-fold-unfold-all
       )
      )
;; Text objects
(map! :map jg-binding-inner-text-objects-map
      :desc "Quote"        "\""  #'evil-inner-double-quote
      :desc "Single-quote" "'"   #'evil-inner-single-quote
      :desc "Paren"        "("   #'evil-inner-paren
      :desc "Angle"        "<"   #'evil-inner-angle
      :desc "Bracket"      "["   #'evil-inner-bracket
      :desc "Curly"        "{"   #'evil-inner-curly
      :desc "back-quote"   "`"   #'evil-inner-back-quote

      :desc "arg"                 "a"   #'evil-inner-arg
      :desc "paren"               "b"   #'evil-inner-paren
      :desc "block"               "B"   #'evil-textobj-anyblock-inner-block
      :desc "comment"             "c"   #'evilnc-inner-comment
      :desc "defun"               "f"   #'+evil:defun-txtobj
      :desc "whole-buffer"        "g"   #'+evil:whole-buffer-txtobj
      :desc "Same Indent Block"   "i"   #'evil-indent-plus-i-indent
      :desc "+Same Indent Block+" "j"   #'evil-indent-plus-i-indent-up-down
      :desc "+Same Indent Block"  "k"   #'evil-indent-plus-i-indent-up
      :desc "Symbol"              "o"   #'evil-inner-symbol
      :desc "Paragraph"           "p"   #'evil-inner-paragraph
      :desc "Any-Quote"           "q"   #'+evil:inner-any-quote
      :desc "Sentence"            "s"   #'evil-inner-sentence
      :desc "XML Tag"             "t"   #'evil-inner-tag
      :desc "URL"                 "u"   #'+evil:inner-url-txtobj
      :desc "WORD"                "W"   #'evil-inner-WORD
      :desc "word"                "w"   #'evil-inner-word
      :desc "XML Attr"            "x"   #'evil-inner-xml-attr
      )
(map! :map jg-binding-outer-text-objects-map
      :desc  "\"" "\""   #'evil-a-double-quote
      :desc  "'"  "'"   #'evil-a-single-quote
      :desc  "`'" "`"   #'evil-a-back-quote
      :desc  "("  "("   #'evil-a-paren
      :desc  ")"  ")"   #'evil-a-paren
      :desc  "<"  "<"   #'evil-an-angle
      :desc  ">"  ">"   #'evil-an-angle
      :desc  "["  "["   #'evil-a-bracket
      :desc  "]"  "]"   #'evil-a-bracket
      :desc  "{"  "{"   #'evil-a-curly
      :desc  "}"  "}"   #'evil-a-curly

      :desc  "outer-arg"                    "a"   #'evil-outer-arg
      :desc  "a-paren"                      "b"   #'evil-a-paren
      :desc  "Paren Block"                  "B"   #'evil-textobj-anyblock-a-block
      :desc  "Commenter"                    "c"   #'evilnc-outer-commenter
      :desc  "defun"                        "f"   #'+evil:defun-txtobj
      :desc  "whole-buffer"                 "g"   #'+evil:whole-buffer-txtobj
      :desc  "Same Indent Block"            "i"   #'evil-indent-plus-a-indent
      :desc  "+Same Indent Block+"          "j"   #'evil-indent-plus-a-indent-up-down
      :desc  "+Same Indent Block"           "k"   #'evil-indent-plus-a-indent-up
      :desc  "Symbol"                       "o"   #'evil-a-symbol
      :desc  "Paragraph"                    "p"   #'evil-a-paragraph
      :desc  "outer-any-quote"              "q"   #'+evil:outer-any-quote
      :desc  "Sentence"                     "s"   #'evil-a-sentence
      :desc  "XML Tag"                      "t"   #'evil-a-tag
      :desc  "URL"                          "u"   #'+evil:outer-url-txtobj
      :desc  "WORD"                         "W"   #'evil-a-WORD
      :desc  "word"                         "w"   #'evil-a-word
      :desc  "XML attr"                     "x"   #'evil-outer-xml-attr
      )
;; My Operators
(map! :map jg-binding-operator-map

      :desc "Repeat Global Sub"   "&"   #'evil-ex-repeat-global-substitute

      :desc "Incr"                "+"   #'evil-numbers/inc-at-pt
      :desc "Decr"                "-"   #'evil-numbers/dec-at-pt
      :desc "Last Change"         ";"   #'goto-last-change
      :desc "Goto Column"         ">"   #'+jg-text-force-column-motion
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
      :desc "Complete/Grow Selection"  "g" (cmds! (eq evil-state 'normal) #'company-manual-begin
                                                  (eq evil-state 'visual) #'+jg-text-grow-selection-op)
      :desc "Insert Resume"      "i" #'evil-insert-resume
      :desc "Join whitespace"    "J" #'evil-join-whitespace

      :desc "Middle of Line"     "m" #'evil-middle-of-visual-line
      :desc "Insert Line"        "o" #'+evil/insert-newline-below
      :desc "Paste"              "p" #'+evil/alt-paste
      :desc "Reselect Region"    "P" #'+evil/reselect-paste
      :desc "Fill"               "q" #'evil-fill-and-move
      :desc "Eval region"        "r" #'+eval:region
      :desc "Replace region"     "R" #'+eval:replace-region

      :desc "Upper"              "U"   #'evil-upcase
      :desc "Down"               "u"   #'evil-downcase
      :desc "Restore selection"  "v"   #'evil-visual-restore
      :desc "Wrap Line"          "w"   #'evil-fill
      :desc "Exchange"           "x"   #'evil-exchange
      :desc "Yank"               "y"   #'+evil:yank-unindented
      :desc "Zap to Char"        "z"   #'zap-up-to-char

      (:prefix ("s" . "String-ops")
       :desc "Rot13"               "'" #'evil-rot13
       :desc "Decode url"         "E"  #'+evil:url-decode
       :desc "Encode url"         "e"  #'+evil:url-encode
       :desc "Inflection"         "i"  #'evil-operator-string-inflection
       :desc "Title Case"         "t"  #'+jg-text-title-case-op

       )
      (:prefix ("/" . "Search")
       :desc "Search Word Forward" "*"  #'evil-ex-search-unbounded-word-forward
       :desc "Goto Definition"      "d" #'evil-goto-definition
       :desc "Lookup"               "D" #'+lookup/references
       :desc "Find File at point"   "F" #'evil-find-file-at-point-with-line
       :desc "Lookup File"          "f" #'+lookup/file
       :desc "Simple Grep"          "g" #'+jg-text-simple-grep-op
       :desc "Next Visual"          "j" #'evil-next-visual-line
       :desc "Previous Visual"      "k" #'evil-previous-visual-line
       :desc "Next Similar String " "s" #'+jg-text-next-similar-string
       :desc "Regexp Builder"       "r" #'regexp-builder

       ;; TODO escalate replace op
       )

      "l" nil
      (:prefix ("l" . "Line-ops")
       ;; TODO uniquify, remove leading whitespace, split on char
       :desc "Justify" "j"                    #'justify-current-line
       :desc "Flush Lines"                "f" #'flush-lines
       :desc "Keep Lines"                 "K" #'keep-lines
       :desc "Delete trailing newlines"   "W" #'doom/delete-trailing-newlines
       :desc "Delete trailing whitespace" "w" #'delete-trailing-whitespace
       :desc "Whitespace Cleanup"         "c" #'whitespace-cleanup
       :desc "Indent"                     "i" #'indent-region
       :desc "Random"                     "r" #'+jg-text-goto-random-line-op
       )
      )
;; Directional Motion
(map! :map jg-binding-backward-motion-map
      :desc "Narrow"       "RET" #'+jg-narrowing-move-focus-backward

      :desc "Section" "["         #'evil-backward-section-begin
      :desc "Close Paren" "]"     #'+jg-text-prev-close-paren-motion

      :desc "Arg"          "a" #'evil-backward-arg
      :desc "Buffer"       "b" #'previous-buffer
      :desc "Comment"      "c" #'+evil/previous-comment
      :desc "Git Hunk"     "d" #'git-gutter:previous-hunk
      :desc "Error"        "e" #'previous-error
      :desc "Heading"      "h" #'outline-previous-visible-heading
      :desc "Empty Line"   "l" #'+jg-text-prev-empty-line-motion
      :desc "Begin Method"       "m" #'+evil/previous-beginning-of-method
      :desc "End Method"       "M" #'+evil/previous-end-of-method
      :desc "Ring Window"  "r" #'window-ring-move-perspective-2
      :desc "Section"      "s" #'evil-backward-section-begin
      :desc "Todo"         "t" #'hl-todo-previous
      :desc "Workspace"    "w" #'+workspace/switch-left

      ;; TODO rotate text
      ;;  (:when (featurep! :editor rotate-text)
      ;; :n "]r"  #'rotate-text
      ;; :n "[r"  #'rotate-text-backward)

      )
(map! :map jg-binding-forward-motion-map
      :desc "Narrow"       "RET" #'+jg-narrowing-move-focus-forward
      :desc "Section"      "]"   #'evil-forward-section-begin
      :desc "Open Section" "["   #'+jg-text-next-open-paren-motion ;; #'evil-forward-section-end

      :desc "Arg"          "a" #'evil-forward-arg
      :desc "Buffer"       "b" #'next-buffer
      :desc "Comment"      "c" #'+evil/next-comment
      :desc "Git Hunk"     "d" #'git-gutter:next-hunk
      :desc "Error"        "e" #'next-error
      :desc "Heading"      "h" #'outline-next-visible-heading
      :desc "Empty Line"   "l" #'+jg-text-next-empty-line-motion
      :desc "Begin Method" "m" #'+evil/next-beginning-of-method
      :desc "End Method"   "M" #'+evil/next-end-of-method
      :desc "Ring Window"  "r" #'window-ring-move-perspective
      :desc "Section"      "s" #'evil-forward-section-begin ;; #'evil-next-flyspell-error
      :desc "Todo"         "t" #'hl-todo-next
      :desc "Workspace"    "w" #'+workspace/switch-right
      )


;; Connect the separate maps together
(map! :map jg-binding-operator-state-map
      :desc "Backward Motion Op"  "[" jg-binding-backward-motion-map
      :desc "Forward Motion Op"   "]" jg-binding-forward-motion-map
      )
(map! :map (jg-binding-normal-state-map jg-binding-visual-state-map jg-binding-motion-state-map)
      :desc "Do Ops"        "g"   jg-binding-operator-map
      :desc "Visual Ops"    "z"   jg-binding-vision-map
      :desc "B Motion" "[" jg-binding-backward-motion-map
      :desc "F Motion"  "]" jg-binding-forward-motion-map
      )
(map! :map (jg-binding-visual-state-map jg-binding-operator-state-map)
      :desc "Inner Select"     "i" jg-binding-inner-text-objects-map
      :desc "Outer Select"     "a" jg-binding-outer-text-objects-map
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
