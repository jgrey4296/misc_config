;;; util/text/+bindings.el -*- lexical-binding: t; -*-

;; Text bindings
(map! :leader
      :prefix "x"
      "l s" #'+jg-text-split-on-char-n
      "s"   #'+jg-text-next-similar-string
      (:prefix ("A" . "Academic Phrases")
       "p"    #'academic-phrases
       "s"    #'academic-phrases-by-section)
)
