;;; emacs/bindings/+evil-maps.el -*- lexical-binding: t; -*-

;; Normal
(map! :map evil-normal-state-map
      "\""  #'evil-use-register
      "& "  #'evil-ex-repeat-substitute
      ". "  #'evil-repeat
      "< "  #'evil-shift-left
      "> "  #'evil-shift-right
      "~"   #'evil-invert-char
      "= "  #'evil-indent
      "@ "  #'evil-execute-macro

      "/"   #'evil-ex-search-forward
      "\\"  #'evil-ex-search-backward

      "i"   #'evil-insert
      "m"   #'evil-set-marker
      "o"   #'evil-open-below
      "p"   #'evil-paste-after
      "q"   #'evil-record-macro
      "r"   #'evil-replace
      "s"   #'evil-substitute
      "u"   #'evil-undo
      "x"   #'evil-delete-char
      "y"   #'evil-yank

      "A "  #'evil-append-line
      "D "  #'evil-delete-line
      "I "  #'evil-insert-line
      "J "  #'evil-join
      "K "  #'+lookup/documentation
      "O "  #'evil-open-above
      "P "  #'evil-paste-before
      "Q "  #'pp-eval-expression
      "R "  #'evil-replace-state
      "S "  #'evil-change-whole-line
      "X "  #'evil-delete-backward-char
      "Y "  #'evil-yank-line
      "Z"   nil

      :desc "Do Ops" "g"   jg-binding-operator-map
      :desc "Visual Ops" "z"   jg-binding-vision-map
      :desc "Backward Motion" "["   jg-binding-backward-motion-map
      :desc "Forward Motion"  "]"   jg-binding-forward-motion-map
)
(map! :map evil-normal-state-map
      "C-."           #'evil-repeat-pop
      "C-n"           #'evil-paste-pop-next
      "C-p"           #'evil-paste-pop
      "C-r"           #'evil-redo
      "C-t"           #'pop-tag-mark
      "M-."           #'evil-repeat-pop-next
      "M-y"           #'evil-paste-pop

      "<deletechar> "         #'evil-delete-char
      "<escape> "             #'evil-force-normal-state
      "<insert> "             #'evil-insert
      "<insertchar> "         #'evil-insert
      "<mouse-2> "            nil ;; #'mouse-yank-primary
      "DEL "                  #'evil-backward-char
      )

;; Visual
(map! :map evil-visual-state-map
      "RET"         #'+jg-text-whole-buffer-textobj
      "."           #'evil-repeat
      "/"           #'evil-ex-search-forward
      "\\"          #'evil-ex-search-backward
      "?"           #'evil-visualstar/begin-search-forward
      "<"           #'+evil/shift-left
      ">"           #'+evil/shift-right
      "<escape>"    #'evil-exit-visual-state
      "C-g"         #'evil-escape
      "@"           #'+evil:apply-macro
      "A"           nil ;; #'evil-append
      "I"           nil ;;#'evil-insert
      "K"           #'+lookup/documentation
      "O"           #'evil-visual-exchange-corners
      "R"           #'evil-change
      "s"           #'evil-surround-region
      "S"           #'evil-surround-change
      "o"           #'exchange-point-and-mark
      "U"           nil ;;#'evil-upcase
      "u"           nil ;;#'evil-downcase
      :desc "Inner Select"     "i" evil-inner-text-objects-map
      :desc "Outer Select"     "a" evil-outer-text-objects-map
      :desc "Do Ops"           "g" jg-binding-operator-map
      :desc "Visual Ops"       "z" jg-binding-vision-map
      :desc "Backward Motion"  "[" jg-binding-backward-motion-map
      :desc "Forward Motion"   "]" jg-binding-forward-motion-map

      "<insert>"    nil ;; undefined
      "<insertchar>"nil ;; undefined
      "<mouse-2>"   nil ;;#'evil-exit-visual-and-repeat
      "#"           nil ;;#'evil-visualstar/begin-search-backward
      "*"           nil ;;#'evil-visualstar/begin-search-forward

      )
;; Operator
(map! :map evil-operator-state-map
      "C-g" #'evil-escape
      "S"   #'evil-Surround-edit
      "s"   #'evil-surround-edit
      "l"   #'+jg-text-line-textobj
      "RET" #'+jg-text-whole-buffer-textobj
      :desc "Inner Select"     "i" evil-inner-text-objects-map
      :desc "Outer Select"     "a" evil-outer-text-objects-map
      :desc "Backward Motion"  "["   jg-binding-backward-motion-map
      :desc "Forward Motion"   "]"   jg-binding-forward-motion-map
      )
;; Motion
(map! :map evil-motion-state-map
      "TAB"           #'indent-for-tab-command
      "RET"           #'evil-ret
      "SPC"           #'evil-forward-char
      "TAB"           #'indent-for-tab-command

      "^"             #'evil-first-non-blank
      "_"             #'evil-next-line-1-first-non-blank
      "`"             #'evil-goto-mark
      ":"             #'evil-ex
      ";"             #'evil-repeat-find-char
      "?"             #'evil-ex-search-backward
      "{"             #'evil-backward-paragraph
      "|"             #'evil-goto-column
      "}"             #'evil-forward-paragraph

      "\\"  nil

      "<left>"  #'evil-scroll-left
      "<right>" #'evil-scroll-right
      "<up>"    #'evil-scroll-page-up
      "<down>"  #'evil-scroll-page-down

      "!"             #'evil-shell-command
      "#"             #'evil-ex-search-word-backward
      "$"             #'evil-end-of-line
      "%"             #'evil-jump-item
      "'"             #'evil-goto-mark-line
      "("             #'evil-backward-sentence-begin
      ")"             #'evil-forward-sentence-begin
      "*"             #'evil-ex-search-word-forward
      "+"             #'evil-next-line-first-non-blank
      ","             #'evil-repeat-find-char-reverse
      "-"             #'evil-previous-line-first-non-blank
      "/"             #'evil-ex-search-forward
      "0"             #'evil-digit-argument-or-evil-beginning-of-line
      "1"             #'digit-argument
      "2"             #'digit-argument
      "3"             #'digit-argument
      "4"             #'digit-argument
      "5"             #'digit-argument
      "6"             #'digit-argument
      "7"             #'digit-argument
      "8"             #'digit-argument
      "9"             #'digit-argument

      "B"             #'evil-backward-WORD-begin
      "E"             #'evil-forward-WORD-end
      "F"             #'evil-find-char-backward
      "G"             #'evil-goto-line
      "H"             #'evil-window-top
      "K"             #'evil-lookup
      "L"             #'evil-window-bottom
      "M"             #'evil-window-middle
      "N"             #'evil-ex-search-previous
      "T"             #'evil-find-char-to-backward
      "V"             #'evil-visual-line
      "W"             #'evil-forward-WORD-begin
      "Y"             #'evil-yank-line

      "b"             #'evil-backward-word-begin
      "e"             #'evil-forward-word-end
      "f"             #'evil-find-char
      "h"             #'evil-backward-char
      "j"             #'evil-next-line
      "k"             #'evil-previous-line
      "l"             #'evil-forward-char
      "n"             #'evil-ex-search-next
      "t"             #'evil-find-char-to
      "v"             #'evil-visual-char
      "w"             #'evil-forward-word-begin
      "y"             #'evil-yank

      :desc "Do Ops"           "g" jg-binding-operator-map
      :desc "Visual Ops"       "z" jg-binding-vision-map
      :desc "Backward Motion"  "[" jg-binding-backward-motion-map
      :desc "Forward Motion"   "]" jg-binding-forward-motion-map
      )

;; Directional Motion
(map! :map jg-binding-backward-motion-map
      :desc "Section" "["       #'evil-backward-section-begin
      :desc "Close Paren" "]"   #'+jg-text-prev-close-paren-motion
      "("   nil  ;; #'evil-previous-open-paren
      "{"   nil  ;; #'evil-previous-open-brace
      :desc "Method" "M"   #'+evil/previous-end-of-method
      :desc "Method"  "m"   #'+evil/previous-beginning-of-method

      :desc "Empty Line"   "SPC" #'+jg-text-prev-empty-line-motion
      :desc "Narrow"  "RET" #'+jg-narrowing-move-focus-backward
      :desc "Arg"       "a"   #'evil-backward-arg
      :desc "Comment"   "c"   #'+evil/previous-comment
      :desc "Git Hunk"  "d"   #'git-gutter:previous-hunk
      :desc "Error"     "e"   #'previous-error
      :desc "Heading"   "h"   #'outline-previous-visible-heading
      :desc "Section"   "s"   #'evil-backward-section-begin
      :desc "Todo"      "t"   #'hl-todo-previous
      :desc "" "F"   nil ;;#'+evil/previous-frame
      :desc "" "SPC" nil ;;#'+evil/insert-newline-above
      :desc "Buffer" "b"   #'previous-buffer
      :desc "" "f"   nil ;; #'+evil/previous-file
      :desc "" "o"   nil ;; #'+evil/insert-newline-above
      :desc "Workspace" "w"   #'+workspace/switch-left

      ;; :desc "Ring Window"  "r"   #'window-ring-move-perspective-2
      "u"   nil ;; #'+evil:url-decode
      "y"   nil ;; #'+evil:c-string-decode
      "#"   nil ;; #'+evil/previous-preproc-directive

)
(map! :map jg-binding-forward-motion-map
      :desc "Narrow"        "RET"        #'+jg-narrowing-move-focus-forward
      :desc "Empty Line"    "SPC"        #'+jg-text-next-empty-line-motion

      :desc "Section"       "]"          #'evil-forward-section-begin
      :desc "Open Paren"    "["          #'+jg-text-next-open-paren-motion ;; #'evil-forward-section-end
      :desc "nothing"       ")"   nil ;; #'evil-next-close-paren
      :desc "nothing"       "}"   nil ;; #'evil-next-close-brace
      :desc "Method"        "M"          #'+evil/next-end-of-method
      :desc ""               "F"   nil ;; #'+evil/next-frame
      :desc "Arg"           "a"          #'evil-forward-arg
      :desc "Buffer"        "b"          #'next-buffer
      :desc "Comment"       "c"          #'+evil/next-comment
      :desc "Git Hunk"      "d"          #'git-gutter:next-hunk
      :desc "Error"         "e"          #'next-error
      :desc "nothing"       "f"   nil ;; #'+evil/next-file
      :desc "Heading"       "h"          #'outline-next-visible-heading
      :desc "Method"        "m"          #'+evil/next-beginning-of-method
      :desc "Section"       "s"          #'evil-forward-section-begin ;; #'evil-next-flyspell-error
      :desc "Todo"          "t"          #'hl-todo-next
      :desc "nothing"       "o"   nil ;; #'+evil/insert-newline-below
      :desc "Workspace"     "w"          #'+workspace/switch-right

       :desc "Ring Window"   "r"         #'window-ring-move-perspective
       "u"   nil ;;                      #'+evil:url-encode
       "y"   nil ;;                      #'+evil:c-string-encode
       "#"   nil ;;                      #'+evil/next-preproc-directive

      )


;; Vision / Hiding
(map! :map jg-binding-vision-map
      :desc "Narrow" "RET" #'+jg-narrow-around-point
      :desc "Widen"  "DEL" #'widen
      "= "          #'ispell-word
      "A"          #'evil-open-fold-rec
      "C"          nil
      "D"          #'evil-close-folds
      "N"          nil ;; #'doom/widen-indirectly-narrowed-buffer
      "O"          nil

      "X"          nil ;; #'kill-current-buffer
      "a"          #'evil-toggle-fold
      "c"          nil
      "d"          #'evil-close-fold
      "m"          nil
      "n"          nil ;; #'+evil:narrow-buffer
      "o"          #'evil-open-fold
      "r"          #'evil-open-folds
      "s"          nil ;;#'evil-open-folds
      :desc "Center" "z" #'evil-scroll-line-to-center
      :desc "Top"    "t" #'evil-scroll-line-to-top
      :desc "Bottom" "b" #'evil-scroll-line-to-bottom

      (:prefix ("'" . "Highlight")
      "."        #'highlight-symbol-at-point
      "f"        #'hi-lock-find-patterns
      "i"        #'hi-lock-write-interactive-patterns
      "l"        #'highlight-lines-matching-regexp
      "p"        #'highlight-phrase
      "r"        #'highlight-regexp
      "u"        #'unhighlight-regexp)

      (:prefix ("v" . "Vimish Fold")
      "A"        #'vimish-fold-toggle-all
      "D"        #'vimish-fold-delete-all
      "a"        #'vimish-fold-toggle
      "d"        #'vimish-fold-delete
      "f"        #'vimish-fold
      "j"        #'vimish-fold-next-fold
      "k"        #'vimish-fold-previous-fold
      "m"        #'vimish-fold-refold-all
      "r"        #'vimish-fold-unfold-all
      "x"          nil)

)
;; Text objects
(map! :map jg-binding-inner-text-objects-map
      :desc "Inner Quote" "\""  #'evil-inner-double-quote
      "'"  #'evil-inner-single-quote
      "("  #'evil-inner-paren
      ")"  #'evil-inner-paren
      "<"  #'evil-inner-angle
      ">"  #'evil-inner-angle
      "B"  #'evil-textobj-anyblock-inner-block
      "W"  #'evil-inner-WORD
      "["  #'evil-inner-bracket
      "]"  #'evil-inner-bracket
      "`"  #'evil-inner-back-quote
      "a"  #'evil-inner-arg
      "b"  #'evil-inner-paren
      "c"  #'evilnc-inner-comment
      "f"  #'+evil:defun-txtobj
      "g"  #'+evil:whole-buffer-txtobj
      "i"  #'evil-indent-plus-i-indent
      "j"  #'evil-indent-plus-i-indent-up-down
      "k"  #'evil-indent-plus-i-indent-up
      "o"  #'evil-inner-symbol
      "p"  #'evil-inner-paragraph
      "q"  #'+evil:inner-any-quote
      "s"  #'evil-inner-sentence
      "t"  #'evil-inner-tag
      "u"  #'+evil:inner-url-txtobj
      "w"  #'evil-inner-word
      "x"  #'evil-inner-xml-attr
      "{"  #'evil-inner-curly
      "}"  #'evil-inner-curly

)
(map! :map jg-binding-outer-text-objects-map
      "\""  #'evil-a-double-quote
      "'"  #'evil-a-single-quote
      "("  #'evil-a-paren
      ")"  #'evil-a-paren
      "<"  #'evil-an-angle
      ">"  #'evil-an-angle
      "B"  #'evil-textobj-anyblock-a-block
      "W"  #'evil-a-WORD
      "["  #'evil-a-bracket
      "]"  #'evil-a-bracket
      "`"  #'evil-a-back-quote
      "a"  #'evil-outer-arg
      "b"  #'evil-a-paren
      "c"  #'evilnc-outer-commenter
      "f"  #'+evil:defun-txtobj
      "g"  #'+evil:whole-buffer-txtobj
      "i"  #'evil-indent-plus-a-indent
      "j"  #'evil-indent-plus-a-indent-up-down
      "k"  #'evil-indent-plus-a-indent-up
      "o"  #'evil-a-symbol
      "p"  #'evil-a-paragraph
      "q"  #'+evil:outer-any-quote
      "s"  #'evil-a-sentence
      "t"  #'evil-a-tag
      "u"  #'+evil:outer-url-txtobj
      "w"  #'evil-a-word
      "x"  #'evil-outer-xml-attr
      "{"  #'evil-a-curly
      "}"  #'evil-a-curly

)

;; My Operators
(map! :map jg-binding-operator-map
      :desc "Repeat Global Sub"   "&"   #'evil-ex-repeat-global-substitute
      :desc "Search Word Forward" "*"   #'evil-ex-search-unbounded-word-forward
      :desc "Incr"                "+"   #'evil-numbers/inc-at-pt
      :desc "Decr"                "-"   #'evil-numbers/dec-at-pt
      :desc "Last Change"         ";"   #'goto-last-change
      :desc "Goto Column"         ">"   #'evil-goto-column
      :desc "Apply Macro"         "@"   #'+evil:apply-macro

      :desc "Pop Mark"            "b" #'avy-pop-mark
      :desc "Comment"             "c" #'evilnc-comment-operator
      :desc "Goto Definition"     "d" #'evil-goto-definition
      :desc "Lookup"              "D" #'+lookup/references
      :desc "Goto First Line"     "f" #'evil-goto-first-line
      :desc "Find File at point"  "F" #'evil-find-file-at-point-with-line
      :desc "Lookup File"         "f" #'+lookup/file
      :desc "Grow Selection"      "g" #'+jg-text-grow-selection-op
      :desc "Insert Resume"       "i" #'evil-insert-resume
      :desc "Next Visual"         "j" #'evil-next-visual-line
      :desc "Join whitespace"     "J" #'evil-join-whitespace
      :desc "Previous Visual"     "k" #'evil-previous-visual-line
      :desc "Middle of Line"      "m" #'evil-middle-of-visual-line
      :desc "Reselect Region"     "P" #'+evil/reselect-paste
      :desc "Paste"               "p" #'+evil/alt-paste
      :desc "Fill"                "q" #'evil-fill-and-move
      :desc "Eval region"         "r" #'+eval:region
      :desc "Replace region"      "R" #'+eval:replace-region
      :desc "Split on Char"       "s" #'+jg-text-split-on-char
      :desc "Title Case"          "t" #'+jg-text-title-case-op
      :desc "Restore selection"   "v" #'evil-visual-restore
      :desc "Fill"                "w" #'evil-fill
      :desc "Exchange"            "x" #'evil-exchange
      :desc "Yank"                "y" #'+evil:yank-unindented
      :desc "Invert"              "~" #'evil-invert-case

      :desc "Zap to Char" "z"   #'zap-up-to-char
      :desc "Insert Line" "SPC" #'+evil/insert-newline-below
      :desc "Char"        "?"   #'what-cursor-position
      :desc "Decode url"  "E"   #'+evil:url-decode
      :desc "Encode url"  "e"   #'+evil:url-encode
      :desc "Line End"    "$"   #'evil-end-of-visual-line
      :desc "Line Start"  "0"   #'evil-beginning-of-visual-line
      :desc "Rot13"       "?"   #'evil-rot13
      )


(+jg-binding-keymap-update-plural jg-binding-operator-map
                                  jg-binding-vision-map
                                  jg-binding-forward-motion-map
                                  jg-binding-backward-motion-map
                                  jg-binding-inner-text-objects-map
                                  jg-binding-outer-text-objects-map)
