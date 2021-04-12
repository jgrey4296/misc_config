;;; util/text/+bindings.el -*- lexical-binding: t; -*-
(message "Loading modules/util/text/+bindings.el")

;; Get rid of zap to char:
(map! "M-z" nil)

;; Text bindings
(map! :n "g '" 'evil-operator-string-inflection)
(map! :leader
      :prefix "x"
      (:prefix ("c" . "Char")
       :desc "Zap" "z" #'zap-up-to-char
       )
      (:prefix ("l" . "Lines")
      :desc "Split on Char N" "s"         #'+jg-text-split-on-char-n
      :desc "Uniqify"         "u"         #'+jg-text-uniquify
      :desc "Remove Leading Whitespace" "L" #'+jg-text-remove-leading-whitespace
      :desc "Jusify"   "j" #'justify-current-line
      )
      (:prefix ("i" . "insert")
       "p"    #'academic-phrases
       "s"    #'academic-phrases-by-section
       )
      (:prefix ("w" . "Word")
       :desc "Title Case" "t" #'+jg-text-title-case
       :desc "Inflection" "i" #'evil-operator-string-inflection
       :desc "Upper"      "U" #'evil-upcase
       :desc "Down"       "u" #'evil-downcase
       )
      (:prefix ("/" . "Search")
       :desc "Next Similar String" "s"       #'+jg-text-next-similar-string
       :desc "Simple Grep"     "g"           #'+jg-text-simple-grep
       )

)
(map! :leader
      :prefix "h"
      :desc "Regex Reminder" "R" #'+jg-text-regex-reminder
      )
