;;; util/text/+bindings.el -*- lexical-binding: t; -*-


(message "Setting up text binding: %s" (current-time-string))
;; Get rid of zap to char:
(map! "M-z" nil)

;; Text bindings
;; (map! :map jg-bindings-operator-map)
(map! :after jg-leader-bindings-loaded
      :map jg-binding-vision-map
      "1" #'+jg-wrap-fold-block
      )

(map! :after jg-leader-bindings-loaded
      :leader
      :prefix "t"
      :n "v r" #'rainbow-mode
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
      )
