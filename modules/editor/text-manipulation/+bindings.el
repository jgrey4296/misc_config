;;; util/text/+bindings.el -*- lexical-binding: t; -*-

(dlog! "Setting up text binding: %s" (current-time-string))
(after! jg-global-bindings
  (keymap-global-set "C-c [" #'+jg-text-insert-lparen)
  (keymap-global-set "C-c ]" #'+jg-text-insert-rparen)
  )
(evil-make-intercept-map messages-buffer-mode-map)
;; Get rid of zap to char:

(map! :leader
      :desc "Clear All"            "r K" #'+jg-text-clear-all
      :desc "Insert Random Word"   "i w" #'+jg-text-insert-random-word

      (:prefix "b"
       :desc "Yank Buffer Name" "n"   #'+jg-text-yank-buffer-name
       :desc "Clear Buffer"     "DEL" #'+jg-text-clear-buffer
       )
      )

(map! :map jg-binding-normal-state-map
      :desc "SPC? Insert" "I SPC"            #'evil-mapspace-state
      :desc "Chars"       "I c"              #'evil-spechar-state
      :desc "Select Whitespace" "v SPC"      #'+jg-text-visual-select-whitespace
      )

(map! :map evil-mapspace-state-map
      "s" #'self-insert-command
      )

(map! :map jg-binding-vision-map
      :prefix ("i" . "Invisible")
      :desc "Add"                       "a"   #'+jg-text-make-invisible
      :desc "Delete"                    "d"   #'+jg-text-delete-invisible
      :desc "Make Comments Invisible"   "c"   #'+jg-text-manipulation-make-comments-invisible
      :desc "Invisibilty Ivy"           "i"   #'+jg-text-manipulate-invis-spec
      :desc "Invisibilty Ivy"           "RET" #'+jg-text-manipulate-invis-spec
      :desc "Named Invisibilty"         "n"   #'+jg-text-name-invisible
      )

(map! :map jg-binding-visual-state-map
      :desc "Grow Selection " "v g"        #'+jg-text-grow-selection-op
      :desc "Select Buffer"   "v RET"      #'+jg-text-whole-buffer-textobj
      :desc "contract"        "v SPC"      #'+jg-text-visual-contract
      :desc "Clone selection" "|"          #'+jg-text-yank-selection-to-new-buffer
      )

(map! :map jg-binding-operator-state-map
      :desc "Select Line"   "g"   #'+jg-text-line-textobj
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

(map! :map jg-binding-outer-text-objects-map
      :desc "Spaces"       "l" #'+jg-text-spaces
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
      :desc "Invis"                       "z"          #'+jg-text-toggle-invisible
      :desc "split line"                  "RET"        #'electric-newline-and-maybe-indent
      :desc "Substitute Ex Memory"        ";"          #'+jg-text-manipulation-sub-memory
      :desc "set buffer coding"           "0"          #'set-buffer-file-coding-system
      :desc "indent"                      "TAB"        #'indent-region


      :desc "Ensure commas"               "u"          #'+jg-surround-ensure-commas

      :desc "Align"                       "a"          #'align-regexp
      :desc "Comment"                     "c"          #'evilnc-comment-operator
      :desc "Surround"                    "s"          #'evil-surround-region

      :desc "Format buffer/region"        "F"          #'+format/region-or-buffer

      :desc "downcase"                    "J"          #'evil-downcase
      :desc "UpperCase"                   "K"          #'evil-upcase
      :desc "Decr"                        "j"          #'+jg-text-dec-num
      :desc "Incr"                        "k"          #'+jg-text-inc-num
      :desc "Shift Left"                  "h"          #'+jg-text-shift-left
      :desc "Remove Indentation"          "H"          #'+jg-text-remove-indentation
      :desc "Shift Right"                 "l"          #'+jg-text-shift-right
      :desc "Title Case"                  "t"          #'+jg-text-title-case-op

      :desc "Quick Change" "\"" #'evil-surround-change
      :desc "Quick Delete" "'"  #'evil-surround-delete
)

(map! :map jg-binding-change-map
      :prefix ("w" . "Words")
      :desc "inflection"                "i"    #'evil-operator-string-inflection
      :desc "Rotate"                    "r"    #'rotate-text
       )

(map! :map jg-binding-change-map :prefix ("e" . "encode")
      :desc "Rot13"                       "r"  #'evil-rot13
      :desc "Encode url"                  "u"  #'+evil:url-encode
      :desc "Decode url"                  "U"  #'+evil:url-decode
      :desc "ENCRYPT"                     "e"  #'+jg-text-encrypt-region
      :desc "DECRYPT"                     "E"  #'+jg-text-decrypt-region
      :desc "Accent"                      "a"  #'accent-menu
      )

(map! :map jg-binding-change-map :prefix ("i" . "lines")
      :desc "Wrap Line"                  "w"   #'evil-fill
      :desc "Fill"                       "W"   #'evil-fill-and-move
      :desc "Combine lines"              "c"   #'evil-join-whitespace
      :desc "Justify"                    "j"   #'justify-current-line
      )

(map! :map jg-binding-change-map :prefix ("o" . "text")
      :desc "Cycle Spacing"               "."  #'cycle-spacing
      :desc "Exchange"                    "x"  #'evil-exchange
      :desc "Split on distance"           "s"  #'+jg-text-split-on-leading-char
      :desc "Title Case"                  "t"  #'+jg-text-title-case-op
      )

(map! :map jg-binding-change-map :prefix ("S" . "Surround")
      "d" #'evil-surround-delete
      "c" #'evil-surround-change
      "l" #'+jg-surround-list
      )

(map! :map jg-binding-operator-map
      :desc "Complete/Grow Selection"     "g" (cmds! (eq evil-state 'normal) #'company-manual-begin
                                                     (eq evil-state 'visual) #'+jg-text-grow-selection-op)
      :desc "Yank"                        "y" #'+evil:yank-unindented
      :desc "Regexp Builder"               "R"    #'regexp-builder
)
(map! :map jg-binding-operator-map :prefix ("d" . "Describe")
      "g" #'writegood-grade-level
      "r" #'writegood-reading-ease
      )

(map! :map jg-binding-operator-map :prefix ("w" . "whitespace")
      :desc "Whitespace clean"            "w" #'+jg-text-run-whitespace-cleanup
      :desc "Delete trailing whitespace"  "W" #'delete-trailing-whitespace
      :desc "Whitespace Cleanup"          "c" #'whitespace-cleanup
      )

(map! :map jg-binding-operator-map :prefix ("f" . "filter")
      :desc "Flush Lines"                "f"   #'flush-lines
      :desc "Keep Lines"                 "k"   #'keep-lines
      :desc "Uniquify"                   "u"   #'delete-duplicate-lines
      :desc "Untabify"                   "TAB" #'untabify
      )

(map! :map jg-binding-operator-map :prefix ("/" . "Search")
      :desc "Simple Grep"          "g" #'+jg-text-simple-grep-op
      :desc "Next Similar String " "s" #'+jg-text-next-similar-string
      )

(map! :map jg-dired-mode-map
      :after jg-dired-bindings
      (:prefix ("c f d" . "Pandoc")
       :desc "Make Style File" "s" #'+jg-text-pandoc-gen-style
       :desc "Compile"         "c" #'+jg-text-pandoc-compile
       )
      )
