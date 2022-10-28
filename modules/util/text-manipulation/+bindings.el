;;; util/text/+bindings.el -*- lexical-binding: t; -*-


(message "Setting up text binding: %s" (current-time-string))
(global-set-key (kbd "C-c [") #'+jg-text-insert-lparen)
(global-set-key (kbd "C-c ]") #'+jg-text-insert-rparen)
;; Get rid of zap to char:
(map! "M-z" nil)

;; Text bindings
(map! :map help-map
      :desc "Regex Reminder" "R" #'+jg-text-regex-reminder
      )

(map! :leader
      (:prefix "b"
       :desc "Yank Buffer Name" "n"   #'+jg-text-yank-buffer-name
       :desc "Clear Buffer"     "DEL" #'+jg-text-clear-buffer
       )
      (:prefix "i"
       :desc "Debug"     "d" #'+jg-text-insert-debug
       )
      )

(map! :map jg-binding-helm-map
      :desc "Word(net)" "w" #'helm-wordnet-suggest
      :desc "Word(nut)" "W" #'wordnut-search
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

(map! :map jg-binding-visual-state-map
      :desc "Select Buffer"   "v RET"      #'+jg-text-whole-buffer-textobj
      :desc "contract"        "v SPC"      #'+jg-text-visual-contract
      )

(map! :map jg-binding-operator-state-map
      :desc "Select Line"   "L"   #'+jg-text-line-textobj
      :desc "Select Buffer" "RET" #'+jg-text-whole-buffer-textobj
      )

(map! :map jg-binding-operator-map
      (:prefix ("s" . "String-ops")
      :desc "Split on distance"        "s" #'+jg-text-split-on-leading-char
      :desc "Set Buffer Coding"        "B" #'set-buffer-file-coding-system
      :desc "ENCRYPT"                  "!" #'+jg-text-encrypt-region
      :desc "DECRYPT"                  "@" #'+jg-text-decrypt-region
      :desc "Uniquify"                 "u" #'delete-duplicate-lines
      :desc "Title Case"               "t"  #'+jg-text-title-case-op
      :desc "Rotate"                   "r" #'rotate-text
      )
      :desc "Goto Column"              "|" #'+jg-text-force-column-motion
      :desc "Complete/Grow Selection"  "g" (cmds! (eq evil-state 'normal) #'company-manual-begin
                                                  (eq evil-state 'visual) #'+jg-text-grow-selection-op)
      (:prefix ("/" . "Search")
       :desc "Simple Grep"          "g" #'+jg-text-simple-grep-op
       :desc "Next Similar String " "s" #'+jg-text-next-similar-string
       )
      (:prefix ("l" . "Line-ops")
       :desc "Random"                     "r" #'+jg-text-goto-random-line-op
       )
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
