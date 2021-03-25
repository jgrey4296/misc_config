;;; util/text/+bindings.el -*- lexical-binding: t; -*-

;; Text bindings
(map! :n "g '" 'evil-operator-string-inflection)
(map! :leader
      :prefix "x"
      :desc "Split on Char N" "l s" #'+jg-text-split-on-char-n
      :desc "Uniqify" "l u" #'+jg-misc-uniquify
      "s"   #'+jg-text-next-similar-string
      (:prefix ("A" . "Academic Phrases")
       "p"    #'academic-phrases
       "s"    #'academic-phrases-by-section)
)
