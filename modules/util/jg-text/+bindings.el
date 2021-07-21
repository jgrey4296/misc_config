;;; util/text/+bindings.el -*- lexical-binding: t; -*-

(message "Setting up text binding: %s" (current-time-string))
;; Get rid of zap to char:
(map! "M-z" nil)

;; Text bindings
;; (map! :map jg-bindings-operator-map)

(map! :leader
      :prefix "t"
       "v r" #'rainbow-mode
      )
(map! :leader
      :prefix "h"
      :desc "Regex Reminder" "R" #'+jg-text-regex-reminder
      )

(map! :map jg-binding-operator-map
      :prefix "s"
      :desc "Split on distance" "s" #'+jg-text-split-on-leading-char
      :desc "Set Buffer Coding" "B" #'set-buffer-file-coding-system
      )
