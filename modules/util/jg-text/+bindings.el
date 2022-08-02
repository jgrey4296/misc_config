;;; util/text/+bindings.el -*- lexical-binding: t; -*-


(message "Setting up text binding: %s" (current-time-string))
(global-set-key (kbd "C-c [") #'+jg-text-insert-lparen)
(global-set-key (kbd "C-c ]") #'+jg-text-insert-rparen)
;; Get rid of zap to char:
(map! "M-z" nil)

;; Text bindings
;; (map! :map jg-bindings-operator-map)
(map! :after jg-leader-bindings-loaded
      :map jg-binding-vision-map
      "1" #'+jg-text-wrap-fold-block
      "i" #'+jg-text-make-invisible
      "I" #'+jg-text-toggle-invisible
      )

(map! :after jg-leader-bindings-loaded
      :leader
      (:prefix "t"
       :desc "Rainbow Mode" :n "v r" #'rainbow-mode
       :desc "Auto-Hide"    :n "h"   #'+jg-text-toggle-auto-hide
       )
      (:prefix "b"
       :desc "Yank Buffer Name" "n"  #'+jg-text-yank-buffer-name
       :desc "Clear Buffer" "DEL"    #'+jg-text-clear-buffer
       )
      (:prefix "i"
       :desc "Debug" "d"             #'+jg-text-insert-debug
       )
      )

(map! :after help
      :map help-map
      :desc "Regex Reminder" "R" #'+jg-text-regex-reminder
      )

(map! :after jg-evil-bindings
      :map jg-binding-operator-map
      :prefix "s"
      :desc "Split on distance" "s" #'+jg-text-split-on-leading-char
      :desc "Set Buffer Coding" "B" #'set-buffer-file-coding-system
      :desc "ENCRYPT"           "!" #'+jg-text-encrypt-region
      :desc "DECRYPT"           "@" #'+jg-text-decrypt-region
      )

(map! :after jg-evil-bindings
      :map jg-binding-visual-state-map
      :desc "Mark Buffer"   "RET"      #'+jg-text-whole-buffer-textobj
      )

(map! :after jg-evil-bindings
      :map jg-binding-operator-state-map
      :desc "Select Line"   "l"   #'+jg-text-line-textobj
      :desc "Select Buffer" "RET" #'+jg-text-whole-buffer-textobj
      )

(map! :after jg-evil-bindings
      :map jg-binding-operator-map
      :desc "Goto Column"              ">" #'+jg-text-force-column-motion
      :desc "Complete/Grow Selection"  "g" (cmds! (eq evil-state 'normal) #'company-manual-begin
                                                  (eq evil-state 'visual) #'+jg-text-grow-selection-op)
      (:prefix ("s" . "String-ops")
       :desc "Title Case"         "t"  #'+jg-text-title-case-op
       )
      (:prefix ("/" . "Search")
       :desc "Simple Grep"          "g" #'+jg-text-simple-grep-op
       :desc "Next Similar String " "s" #'+jg-text-next-similar-string
       )
      (:prefix ("l" . "Line-ops")
       :desc "Random"                     "r" #'+jg-text-goto-random-line-op
       )
      )

(map! :after jg-evil-bindings
      :map jg-binding-backward-motion-map
      :desc "Close Paren"  "]"   #'+jg-text-prev-close-paren-motion
      :desc "Empty Line"   "l"   #'+jg-text-prev-empty-line-motion
      )

(map! :after jg-evil-bindings
      :map jg-binding-forward-motion-map
      :desc "Open Section" "["   #'+jg-text-next-open-paren-motion ;; #'evil-forward-section-end
      :desc "Empty Line"   "l" #'+jg-text-next-empty-line-motion
      )
