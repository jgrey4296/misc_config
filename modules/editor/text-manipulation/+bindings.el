;;; util/text/+bindings.el -*- lexical-binding: t; -*-

(message "Setting up text binding: %s" (current-time-string))
(global-set-key (kbd "C-c [") #'+jg-text-insert-lparen)
(global-set-key (kbd "C-c ]") #'+jg-text-insert-rparen)
;; Get rid of zap to char:
(map! "M-z" nil)

;; Text bindings
(map! :map jg-binding-help-map
      :desc "Regex Reminder" "R" #'+jg-text-regex-reminder
      )

(map! :leader
      (:prefix "b"
       :desc "Yank Buffer Name" "n"   #'+jg-text-yank-buffer-name
       :desc "Clear Buffer"     "DEL" #'+jg-text-clear-buffer
       )

      (:prefix "i"
       :desc "Debug"     "d" #'+jg-text-insert-debug
       :desc "License"   "L" #'license-templates-insert

       (:prefix ("l" . "Lorem Ipsum")
        :desc "Sentence"         "s" #'lorem-ipsum-insert-sentences
        :desc "Paragraph"        "p" #'lorem-ipsum-insert-paragraphs
        :desc "List"             "l" #'lorem-ipsum-insert-list
        :desc "Academic"         "a" #'academic-phrases
        :desc "Academic Section" "A" #'academic-phrases-by-section
        )
       )
      )

(map! :map jg-binding-normal-state-map
      :desc "Debug"          "I d"   #'+jg-text-insert-debug

      :desc "Rotate"         "R"   #'rotate-text
      :desc "Zap to Char"    "Z"   #'zap-up-to-char
      (:prefix "c"
       :desc "Title Case"     "t"   #'+jg-text-title-case-op
       )

      )

(map! :map jg-binding-vision-map
      :prefix ("i" . "Invisible")
      :desc "Add"            "a" #'+jg-text-make-invisible
      :desc "Delete "        "d" #'+jg-text-delete-invisible
      :desc "Invisible-spec" "i" #'+jg-text-toggle-invisible-spec
      )

(map! :map jg-binding-normal-state-map
      :after undo-fu
      :desc "undo" "u" 'undo-fu-only-undo
      :desc "redo" "U" 'undo-fu-only-redo
      )

(map! :map jg-binding-visual-state-map
      :desc "Select Buffer"   "v RET"      #'+jg-text-whole-buffer-textobj
      :desc "contract"        "v SPC"      #'+jg-text-visual-contract
      :desc "Clone selection" "|"          #'+jg-text-yank-selection-to-new-buffer
      )

(map! :map jg-binding-operator-state-map
      :desc "Select Line"   "L"   #'+jg-text-line-textobj
      :desc "Select Buffer" "RET" #'+jg-text-whole-buffer-textobj
      )

(map! :map jg-binding-backward-operator-motion-map
      :desc "Close Paren"  "]"   #'+jg-text-prev-close-paren-motion
      :desc "Empty Line"   "l"   #'+jg-text-prev-empty-line-motion
      )

(map! :map jg-binding-forward-operator-motion-map
      :desc "Open Section" "["   #'+jg-text-next-open-paren-motion
      :desc "Empty Line"   "l"   #'+jg-text-next-empty-line-motion
      )

(map! :map jg-binding-inner-text-objects-map
      :desc "Empty lines"  "l" #'+jg-text-blank-block

      )

(map! :map jg-binding-jump-map
      :desc "Force Goto Column" "\\" #'+jg-text-force-column-motion
      :desc "Random Line "      "?" #'+jg-text-goto-random-line-op
      )

(map! :map jg-binding-motion-state-map
      :desc "Goto Column"       "\\" #'+jg-text-column-motion
      )

(map! :map license-mode-map
      :localleader
      :desc "License Reference" "1" (cmd! (browse-url "https://choosealicense.com/licenses/"))
      )

(map! :map jg-binding-change-map
      :desc "Split Line"                  "RET" #'electric-newline-and-maybe-indent
      :desc "Set Buffer Coding"          "0"   #'set-buffer-file-coding-system
      :desc "Indent"                     "TAB" #'indent-region

      :desc "Align"                       "a" #'align-regexp
      :desc "Comment"                     "c" #'evilnc-comment-operator
      :desc "Surround"                    "s" #'evil-surround-region

      :desc "downcase"                    "j" #'evil-downcase
      :desc "UpperCase"                   "k" #'evil-upcase
      :desc "Decr"                        "J" #'+jg-text-dec-num
      :desc "Incr"                        "K" #'+jg-text-inc-num
      :desc "Shift Left"                  "h" #'+jg-text-shift-left
      :desc "Shift Right"                 "l" #'+jg-text-shift-right

      (:prefix ("t" . "text")
       :desc "Cycle Spacing"               "." #'cycle-spacing
       :desc "Exchange"                    "x" #'evil-exchange

       :desc "Split on distance"          "s" #'+jg-text-split-on-leading-char
       :desc "Title Case"                 "t" #'+jg-text-title-case-op
       )

      (:prefix ("l" . "lines")
       :desc "Wrap Line"                  "w" #'evil-fill
       :desc "Fill"                       "W" #'evil-fill-and-move
       :desc "Combine lines"              "c" #'evil-join-whitespace
       :desc "Justify"                    "j" #'justify-current-line
       )

      (:prefix ("w" . "Words")
       :desc "ispell-word"               "s" #'ispell-word
       :desc "inflection"                "i" #'evil-operator-string-inflection
       :desc "Rotate"                    "r" #'Rotate-Text
       )

      (:prefix ("e" . "encode")
       :desc "Rot13"                       "r" #'evil-rot13
       :desc "Encode url"                  "u" #'+evil:url-encode
       :desc "Decode url"                  "U" #'+evil:url-decode
       :desc "ENCRYPT"                     "e" #'+jg-text-encrypt-region
       :desc "DECRYPT"                     "E" #'+jg-text-decrypt-region
      )

      )

(map! :map jg-binding-operator-map
      :desc "Vundo"                       "u" #'vundo
      :desc "Complete/Grow Selection"     "g" (cmds! (eq evil-state 'normal) #'company-manual-begin
                                                     (eq evil-state 'visual) #'+jg-text-grow-selection-op)

       :desc "Yank"                        "y" #'+evil:yank-unindented

       (:prefix ("w" . "whitespace")
        :desc "Whitespace clean"            "w" #'+jg-text-run-whitespace-cleanup
        :desc "Delete trailing whitespace"  "W" #'delete-trailing-whitespace
        :desc "Whitespace Cleanup"          "c" #'whitespace-cleanup
       )

      (:prefix ("f" . "filter")
       :desc "Flush Lines"                "f"   #'flush-lines
       :desc "Keep Lines"                 "k"   #'keep-lines
       :desc "Uniquify"                   "u"   #'delete-duplicate-lines
       :desc "Untabify"                   "TAB" #'untabify
       )

      (:prefix ("i" . "info")
       :desc "Word(net)"                 "w" #'helm-wordnet-suggest
       :desc "Word(nut)"                 "W" #'wordnut-search
       )

      (:prefix ("/" . "Search")
       :desc "Simple Grep"          "g" #'+jg-text-simple-grep-op
       :desc "Next Similar String " "s" #'+jg-text-next-similar-string
       )

      )

(map! :map jg-dired-mode-map
      (:prefix ("> d" . "Pandoc")
       :desc "Make Style File" "s" #'+jg-text-pandoc-gen-style
       :desc "Compile"          "c" #'+jg-text-pandoc-compile
       )
      (:prefix ("> j" . "jq")
       :desc "Format" "f" #'+jg-text-jq-format
       :desc "Manual" "?" (cmd! (+jg-browse-url "https://stedolan.github.io/jq/manual/"))
       :desc "Expr"   "e" #'+jg-text-jq-expr
       )
      )
