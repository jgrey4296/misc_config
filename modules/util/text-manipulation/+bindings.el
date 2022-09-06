;;; util/text/+bindings.el -*- lexical-binding: t; -*-


(message "Setting up text binding: %s" (current-time-string))
(global-set-key (kbd "C-c [") #'+jg-text-insert-lparen)
(global-set-key (kbd "C-c ]") #'+jg-text-insert-rparen)
;; Get rid of zap to char:
(map! "M-z" nil)

;; Text bindings
(map! :map help-map
      :after jg-help-bindings
      :desc "Regex Reminder" "R" #'+jg-text-regex-reminder
      )

(map! :after jg-leader-bindings-loaded
      :leader
      (:prefix "t"
       :desc "Rainbow Mode"   :n "v r" #'rainbow-mode
       :desc "Invisible-spec" :n "v I" #'+jg-text-toggle-invisible-spec
       )
      (:prefix "b"
       :desc "Yank Buffer Name" "n"  #'+jg-text-yank-buffer-name
       :desc "Clear Buffer" "DEL"    #'+jg-text-clear-buffer
       )
      (:prefix "i"
       :desc "Debug" "d"             #'+jg-text-insert-debug
       )
      )

(map! :map jg-binding-normal-state-map
      :after jg-evil-bindings
      :desc "Debug"          "i d"   #'+jg-text-insert-debug
      :desc "Select Buffer"  "v RET" #'+jg-text-whole-buffer-textobj

      :desc "Title Case"     "c t"   #'+jg-text-title-case-op
      :desc "Rotate"         "c r"   #'rotate-text
      )


(map! :map jg-binding-vision-map
      :after jg-leader-bindings-loaded
      "i" #'+jg-text-make-invisible
      "I" #'+jg-text-toggle-invisible
      )

(map! :map jg-binding-visual-state-map
      :after jg-evil-bindings
      :desc "Select Buffer"   "v RET"      #'+jg-text-whole-buffer-textobj
      )

(map! :map jg-binding-operator-state-map
      :after jg-evil-bindings
      :desc "Select Line"   "L"   #'+jg-text-line-textobj
      :desc "Select Buffer" "RET" #'+jg-text-whole-buffer-textobj
      )

(map! :map jg-binding-operator-map
      :after jg-evil-bindings
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
      :after jg-evil-bindings
      :desc "Close Paren"  "]"   #'+jg-text-prev-close-paren-motion
      :desc "Empty Line"   "l"   #'+jg-text-prev-empty-line-motion
      )

(map! :map jg-binding-forward-operator-motion-map
      :after jg-evil-bindings
      :desc "Open Section" "["   #'+jg-text-next-open-paren-motion
      :desc "Empty Line"   "l"   #'+jg-text-next-empty-line-motion
      )

(map! :map jg-binding-inner-text-objects-map
      ;; :after jg-evil-bindings
      :desc "Empty lines"  "l" #'+jg-text-blank-block

      )
