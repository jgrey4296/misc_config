;;; util/text/+bindings.el -*- lexical-binding: t; -*-
(message "Loading modules/util/text/+bindings.el")

;; Get rid of zap to char:
(map! "M-z" nil)

;; Text bindings
;; (map! :map jg-bindings-operator-map)

(map! :leader
      :prefix "x"
      (:prefix ("c" . "Char"))
      (:prefix ("l" . "Lines"))
      (:prefix ("w" . "Word"))
      (:prefix ("/" . "Search"))
)
(map! :leader
      :prefix "h"
      :desc "Regex Reminder" "R" #'+jg-text-regex-reminder
      )
