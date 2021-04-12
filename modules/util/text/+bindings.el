;;; util/text/+bindings.el -*- lexical-binding: t; -*-
(message "Loading modules/util/text/+bindings.el")

;; Text bindings
(map! :n "g '" 'evil-operator-string-inflection)
(map! :leader
      :prefix "x"
      :desc "Split on Char N" "l s"         #'+jg-text-split-on-char-n
      :desc "Uniqify"         "l u"         #'+jg-misc-uniquify
      :desc "Next Similar String" "s"       #'+jg-text-next-similar-string
      :desc "Simple Grep"     "l m"         #'+jg-text-simple-grep
      (:prefix ("A" . "Academic Phrases")
       "p"    #'academic-phrases
       "s"    #'academic-phrases-by-section)
)
(map! :leader
      :prefix "h"
      :desc "Regex Reminder" "R" #'+jg-text-regex-reminder
      )
