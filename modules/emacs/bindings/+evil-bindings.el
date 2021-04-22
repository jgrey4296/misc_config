;;; emacs/bindings/+evil-maps.el -*- lexical-binding: t; -*-

;; Evil States
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
      "R "  #'evil-replace-state
      "S "  #'evil-change-whole-line
      "X "  #'evil-delete-backward-char
      "Y "  #'evil-yank-line
      "Z"   nil
)

(map! :map evil-normal-state-map
      "C-. "           #'evil-repeat-pop
      "C-n "           #'evil-paste-pop-next
      "C-p "           #'evil-paste-pop
      "C-r "           #'evil-redo
      "C-t "           #'pop-tag-mark
      "M-. "           #'evil-repeat-pop-next
      "M-y "           #'evil-paste-pop

      "<deletechar> "          #'evil-delete-char
      "<escape> "             #'evil-force-normal-state
      "<insert> "             #'evil-insert
      "<insertchar> "         #'evil-insert
      "<mouse-2> "            nil ;; #'mouse-yank-primary
      "DEL "                  #'evil-backward-char

      "[ F"          #'+evil/previous-frame
      "[ SPC"        #'+evil/insert-newline-above
      "[ b"          #'previous-buffer
      "[ f"          #'+evil/previous-file
      "[ o"          #'+evil/insert-newline-above
      "[ w"          #'+workspace/switch-left
      "] F"          #'+evil/next-frame
      "] SPC"        #'+evil/insert-newline-below
      "] b"          #'next-buffer
      "] f"          #'+evil/next-file
      "] o"          #'+evil/insert-newline-below
      "] w"          #'+workspace/switch-right

      )
;; <g>
(map! :map evil-normal-state-map
      :prefix ("z" . "Hiding / Hightlight")
      "= "          #'ispell-word
      "A"          #'evil-open-fold-rec
      "C"          nil
      "D"          #'evil-close-folds
      "N"          nil ;; #'doom/widen-indirectly-narrowed-buffer
      "O"          nil
      "RET"        #'+jg-narrow-around-point
      "X"          nil ;; #'kill-current-buffer
      "a"          #'evil-toggle-fold
      "c"          nil
      "d"          #'evil-close-fold
      "m"          nil
      "n"          nil ;; #'+evil:narrow-buffer
      "o"          #'evil-open-fold
      "r"          #'evil-open-folds
      "s"          nil ;;#'evil-open-folds
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
;; <z>
(map! :map evil-normal-state-map
      :prefix ("g" . "Do Op")
      "&"           #'evil-ex-repeat-global-substitute
      ","           #'goto-last-change-reverse
      "-"           #'evil-numbers/dec-at-pt
      "8"           nil ;; #'what-cursor-position
      ";"           #'goto-last-change
      "="           #'evil-numbers/inc-at-pt
      ">"           #'evil-goto-column
      :desc "Rot13" "?"           #'evil-rot13
      "@"           #'+evil:apply-macro
      "A"           #'+lookup/assignments
      "D"           #'+lookup/references
      "F"           #'evil-find-file-at-point-with-line
      "I"           #'+lookup/implementations
      "J"           #'evil-join-whitespace
      "L"           #'evil-lion-right
      "R"           nil ;; #'+eval/buffer
      "T"           nil ;; #'+workspace:switch-previous
      "U"           #'evil-upcase
      "a"           #'what-cursor-position
      "b"           #'avy-pop-mark
      "c"           #'evilnc-comment-operator
      "d"           #'+lookup/definition
      "f"           nil ;; #'+lookup/file
      "i"           #'evil-insert-resume
      "l"           #'evil-lion-left
      "p"           #'+evil/reselect-paste
      "q"           #'evil-fill-and-move
      "r"           nil ;; #'+eval:region
      "t"           nil ;; #'+workspace:switch-next
      "u"           #'evil-downcase
      "w"           #'evil-fill
      "x"           #'evil-exchange
      "y"           #'+evil:yank-unindented
      "~"           #'evil-invert-case

)

(map! :map evil-visual-state-map
      "#"           nil ;;#'evil-visualstar/begin-search-backward
      "*"           nil ;;#'evil-visualstar/begin-search-forward
      "/"           #'evil-ex-search-forward
      "\\"          #'evil-ex-search-backward
      "?"           #'evil-visualstar/begin-search-forward
      "<"           #'+evil/shift-left
      "<escape>"    #'evil-exit-visual-state
      "<insert>"    nil ;; undefined
      "<insertchar>"nil ;; undefined
      "<mouse-2>"   nil ;;#'evil-exit-visual-and-repeat
      ">"           #'+evil/shift-right
      "@"           #'+evil:apply-macro
      "A"           nil ;; #'evil-append
      "C-g"         #'evil-escape
      "I"           nil ;;#'evil-insert
      "K"           #'+lookup/documentation
      "O"           #'evil-visual-exchange-corners
      "R"           #'evil-change
      "S"           #'evil-surround-region
      "U"           #'evil-upcase
      "o"           #'exchange-point-and-mark
      "u"           #'evil-downcase
      (:prefix "z"
       "="         #'ispell-word
       ;;"n"         #'+evil:narrow-buffer
       "n" nil
       )
      )
;; <a>
(map! :map evil-visual-state-map
      :prefix ("a" . "Outer Select")
      "\""    #'evil-a-double-quote
      "'"     #'evil-a-single-quote
      "("     #'evil-a-paren
      ")"     #'evil-a-paren
      "<"     #'evil-an-angle
      ">"     #'evil-an-angle
      "["     #'evil-a-bracket
      "]"     #'evil-a-bracket
      "{"     #'evil-a-curly
      "}"     #'evil-a-curly
      "`"     #'evil-a-back-quote
      "B"     #'evil-textobj-anyblock-a-block
      "W"     #'evil-a-WORD

      "a"     #'evil-outer-arg
      "b"     #'evil-a-paren
      "c"     #'evilnc-outer-commenter
      "f"     #'+evil:defun-txtobj
      "g"     #'+evil:whole-buffer-txtobj
      "i"     #'evil-indent-plus-a-indent
      "j"     #'evil-indent-plus-a-indent-up-down
      "k"     #'evil-indent-plus-a-indent-up
      "o"     #'evil-a-symbol
      "p"     #'evil-a-paragraph
      "q"     #'+evil:outer-any-quote
      "s"     #'evil-a-sentence
      "t"     #'evil-a-tag
      "u"     #'+evil:outer-url-txtobj
      "w"     #'evil-a-word
      "x"     #'evil-outer-xml-attr
      )
;; <g>
(map! :map evil-visual-state-map
      :prefix ("g" . "Do Op")
      "+"       #'evil-numbers/inc-at-pt
      "-"       #'evil-numbers/dec-at-pt-incremental
      "="       #'evil-numbers/inc-at-pt-incremental
      "@"       #'+evil:apply-macro
      "A"       #'+lookup/assignments
      "D"       #'+lookup/references
      "I"       #'+lookup/implementations
      "L"       #'evil-lion-right
      "R"       #'+eval:replace-region
      "c"       #'evilnc-comment-operator
      "d"       #'+lookup/definition
      "f"       #'+lookup/file
      "l"       #'evil-lion-left
      "p"       #'+evil/alt-paste
      "r"       #'+eval:region
      "x"       #'evil-exchange
      "y"       #'+evil:yank-unindented
      )
;; <i>
(map! :map evil-visual-state-map
      :prefix ("i" . "Inner Selection")
      "\""        #'evil-inner-double-quote
      "'"         #'evil-inner-single-quote
      "("         #'evil-inner-paren
      ")"         #'evil-inner-paren
      "<"         #'evil-inner-angle
      ">"         #'evil-inner-angle
      "B"         #'evil-textobj-anyblock-inner-block
      "W"         #'evil-inner-WORD
      "["         #'evil-inner-bracket
      "]"         #'evil-inner-bracket
      "`"         #'evil-inner-back-quote
      "a"         #'evil-inner-arg
      "b"         #'evil-inner-paren
      "c"         #'evilnc-inner-comment
      "f"         #'+evil:defun-txtobj
      "g"         #'+evil:whole-buffer-txtobj
      "i"         #'evil-indent-plus-i-indent
      "j"         #'evil-indent-plus-i-indent-up-down
      "k"         #'evil-indent-plus-i-indent-up
      "o"         #'evil-inner-symbol
      "p"         #'evil-inner-paragraph
      "q"         #'+evil:inner-any-quote
      "s"         #'evil-inner-sentence
      "t"         #'evil-inner-tag
      "u"         #'+evil:inner-url-txtobj
      "w"         #'evil-inner-word
      "x"         #'evil-inner-xml-attr
      "{"         #'evil-inner-curly
      "}"         #'evil-inner-curly
)

(map! :map evil-operator-state-map
      "C-g" #'evil-escape
      "S"   #'evil-Surround-edit
      "s"   #'evil-surround-edit
      )
;; <a>
(map! :map evil-operator-state-map
      :prefix ("a" . "Outer Select")
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
;; <i>
(map! :map evil-operator-state-map
      :prefix ("i" . "Inner Select")
      "\"" #'evil-inner-double-quote
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

(map! :map evil-motion-state-map
      ;;"TAB" nil
      "TAB"     #'indent-for-tab-command
      "\\"  nil
      ;; TODO possibly these should be in operator map:
      "] RET"   #'+jg-narrowing-move-focus-forward
      "[ RET"   #'+jg-narrowing-move-focus-backward
      "g b"     #'avy-pop-mark
      "g >"     #'evil-goto-column
      "<left>"  #'evil-scroll-left
      "<right>" #'evil-scroll-right
      "<up>"    #'evil-scroll-page-up
      "<down>"  #'evil-scroll-page-down
      ;; "ESC <down>" #'(lambda () (interactive) (message "Test"))
)
;; <g>
(map! :map evil-motion-state-map
      :prefix ("g" . "Do Op")
      ;; TODO: count-words evil-first-non-blank-of-visual-line evil-last-non-blank
      "#"             nil ;; evil-ex-search-unbounded-word-backward
      :desc "Line Start" "0"             #'evil-beginning-of-visual-line
      :desc "Line End"   "$"             #'evil-end-of-visual-line
      "d "            #'evil-goto-definition
      "*"             #'evil-ex-search-unbounded-word-forward
      "f "            #'evil-goto-first-line
      "j "            #'evil-next-visual-line
      "k "            #'evil-previous-visual-line
      "m "            #'evil-middle-of-visual-line
      "v "            #'evil-visual-restore

      "C-]"           nil ;; evil-jump-to-tag
      "C-g "          nil ;; count-words
      "E "            nil ;; evil-backward-WORD-end
      "N "            nil ;; evil-previous-match
      "^ "            nil ;; evil-first-non-blank-of-visual-line
      "_ "            nil ;; evil-last-non-blank
      "e "            nil ;; evil-backward-word-end
      "g "            nil ;; #'evil-goto-first-line
      "n "            nil ;; #'evil-next-match
      "s "            nil
)
      ;; "s # "          nil ;; #'evilem-motion-search-word-backward
      ;; "s ( "          nil ;; #'evilem-motion-backward-sentence-begin
      ;; "s ) "          nil ;; #'evilem-motion-forward-sentence-begin
      ;; "s * "          nil ;; #'evilem-motion-search-word-forward
      ;; "s + "          nil ;; #'evilem-motion-next-line-first-non-blank
      ;; "s - "          nil ;; #'evilem-motion-previous-line-first-non-blank
      ;; "s / "          nil ;; #'evil-avy-goto-char-timer
      ;; "s A "          nil ;; #'evilem--motion-function-evil-backward-arg
      ;; "s B "          nil ;; #'evilem-motion-backward-WORD-begin
      ;; "s E "          nil ;; #'evilem-motion-forward-WORD-end
      ;; "s F "          nil ;; #'evilem-motion-find-char-backward
      ;; "s N "          nil ;; #'evilem-motion-search-previous
      ;; "s SPC "        nil
      ;; "s T "          nil ;; #'evilem-motion-find-char-to-backward
      ;; "s W "          nil ;; #'evilem-motion-forward-WORD-begin
      ;; "s ["           nil
      ;; "s [ [ "        nil ;; #'evilem-motion-backward-section-begin
      ;; "s [ ] "        nil ;; #'evilem-motion-backward-section-end
      ;; "s ] "          nil
      ;; "s ] [ "        nil ;; #'evilem-motion-forward-section-end
      ;; "s ] ] "        nil ;; #'evilem-motion-forward-section-begin
      ;; "s a "          nil ;; #'evilem--motion-function-evil-forward-arg
      ;; "s b "          nil ;; #'evilem-motion-backward-word-begin
      ;; "s e "          nil ;; #'evilem-motion-forward-word-end
      ;; "s f "          nil ;; #'evilem-motion-find-char
      ;; "s g E "        nil ;; #'evilem-motion-backward-WORD-end
      ;; "s g e "        nil ;; #'evilem-motion-backward-word-end
      ;; "s g j "        nil ;; #'evilem-motion-next-visual-line
      ;; "s g k "        nil ;; #'evilem-motion-previous-visual-line
      ;; "s j "          nil ;; #'evilem-motion-next-line
      ;; "s k "          nil ;; #'evilem-motion-previous-line
      ;; "s n "          nil ;; #'evilem-motion-search-next
      ;; "s s "          nil ;; #'evil-avy-goto-char-2
      ;; "s t "          nil ;; #'evilem-motion-find-char-to
      ;; "s w "          nil ;; #'evilem-motion-forward-word-begin
;; <z>
(map! :map evil-motion-state-map
      :prefix "z"
      "n" nil
      "+" nil
      "-" nil
      "." nil
      "^" nil
      "n" nil
      "N" nil
      "o" nil
      "s" nil
      "X" nil
      :desc "Narrow" "RET" #'+jg-narrow-around-point
      :desc "Widen"  "DEL" #'widen
      )

(map! :map evil-snipe-mode-map
      :nm "S" nil
      :nm "s" nil
      )
