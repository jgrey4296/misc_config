;;; config/default/+bindings.el -*- lexical-binding: t; -*-

(message "Setting up Evil Submaps")

(map! :map jg-binding-insert-state-map
      "C-j"    #'next-line
      "C-k"    #'previous-line
      )

;; For folks with `evil-collection-setup-minibuffer' enabled
(define-key! :states 'insert :keymaps +default-minibuffer-maps
  "C-j"    #'next-line
  "C-k"    #'previous-line)

(define-key! read-expression-map
  "C-j" #'next-line-or-history-element
  "C-k" #'previous-line-or-history-element)


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
;
;;-- operators
(map! :map jg-binding-operator-map
      ;; g > s
      :desc "Repeat Global Sub"   "&"   #'evil-ex-repeat-global-substitute

      :desc "Incr"                "="   #'evil-numbers/inc-at-pt
      :desc "Incr"                "+"   #'evil-numbers/inc-at-pt
      :desc "Decr"                "-"   #'evil-numbers/dec-at-pt
      :desc "Apply Macro"         "@"   #'+evil:apply-macro
      :desc "Char"                "?"   #'what-cursor-position

      :desc "Align"              "a" #'align-regexp
      :desc "IEdit"              "e" #'iedit-mode
      :desc "helms"              "h" 'jg-binding-helm-map

      :desc "Join whitespace"    "J" #'evil-join-whitespace


      )
(map! :map jg-binding-operator-map
      :prefix ("s" . "String-ops")
       ;; t
       :desc "Rot13"              "'" #'evil-rot13
       :desc "Decode url"         "E" #'+evil:url-decode
       :desc "Encode url"         "e" #'+evil:url-encode
       :desc "Inflection"         "i" #'evil-operator-string-inflection
       :desc "ispell-word"        "=" #'ispell-word

       :desc "Fill"               "q" #'evil-fill-and-move
       :desc "Exchange"           "x" #'evil-exchange
       :desc "Yank"               "y" #'+evil:yank-unindented
       )
(map! :map jg-binding-operator-map
      :prefix ("l" . "Line-ops")
      ;; TODO split on char
      ;; r
      :desc "Wrap Line"                  "w"   #'evil-fill

      :desc "Justify"                    "j" #'justify-current-line
      :desc "Flush Lines"                "f" #'flush-lines
      :desc "Indent"                     "i" #'indent-region
      :desc "Keep Lines"                 "K" #'keep-lines
      :desc "Whitespace Cleanup"         "c" #'whitespace-cleanup
      :desc "Untabify"                   "u" #'untabify

      )
;;-- end operators

;;-- jump
(map! :map jg-binding-jump-map
      ;; 1 2 3 "u" "h"
      :desc "Last Change"           ";" #'goto-last-change
      :desc "Jump to Char"          "." #'avy-goto-char
      :desc "Line"                  "l" #'evil-avy-goto-line
      :desc "Middle of Line"        "M" #'evil-middle-of-visual-line

      :desc "Jump to mark"          "m" #'evil-show-marks
      :desc "Pop Mark"              "b" #'avy-pop-mark
      :desc "Push Mark"             "B" (cmd! (avy-push-mark))
      :desc "Goto First Line"       "f" #'evil-goto-first-line
      :desc "Goto Last Line"        "F" #'evil-goto-line
      :desc "Search buffer"         "s" #'swiper
      )
(map! :map jg-binding-jump-map
      :desc "Type definition"       "t" #'+lookup/type-definition
      :desc "References"            "r" #'+lookup/references
      :desc "Definition"            "d" #'+lookup/definition
      :desc "Implementations"       "i" #'+lookup/implementations
      :desc "Documentation"         "k" #'+lookup/documentation
      )
(map! :map jg-binding-jump-map
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
(map! :map jg-binding-jump-map
      :prefix ("/" . "Search")
      :desc "Find File at point"   "F" #'evil-find-file-at-point-with-line
      :desc "Goto Definition"      "d" #'evil-goto-definition
      :desc "Lookup File"          "f" #'+lookup/file
      :desc "Lookup"               "D" #'+lookup/references
      :desc "Next Visual"          "j" #'evil-next-visual-line
      :desc "Previous Visual"      "k" #'evil-previous-visual-line
      :desc "Search Word Forward"  "*"  #'evil-ex-search-unbounded-word-forward
      )
;;-- end jump

;;-- helm
(map! :map jg-binding-helm-map
      :desc "Minibuffer History"           "m"   #'counsel-minibuffer-history
      :desc "Shell History"                "s"   #'counsel-shell-history
      :desc "Helm Processes"               "h"   #'helm-list-emacs-process
      )

;;-- end helm

;;-- vision
(map! :map jg-binding-vision-map
      :desc "Visual Mark Mode"    "0" #'evil-visual-mark-mode
      ;; RET, 1, aAdocrjkIi
      :desc "Widen"         "DEL" #'widen
      :desc "Widen"         "w"   #'widen
      :desc "Scroll Right"  "l"   #'evil-scroll-column-right
      :desc "Scroll Left"   "h"   #'evil-scroll-column-left


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

(provide 'jg-binding-submaps)
