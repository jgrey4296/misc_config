;;; +bindings.el -*- lexical-binding: t; -*-

(global-set-key [remap ispell-word] #'+spell/correct)


(map! :map jg-binding-change-map
      :prefix ("w" . "Words")
       :desc "ispell-word"               "s" #'ispell-word
       :desc "add word to dict"          "a" #'+spell/add-word
       :desc "Word(net)"                 "w" #'helm-wordnet-suggest
       :desc "Word(nut)"                 "W" #'wordnut-search
       )

;; (map! :map jg-binding-forward-general-motion-map
;;       "s"   #'+spell/next-error
;;       )
;; (map! :map jg-binding-backward-general-motion-map
;;       "s"   #'+spell/previous-error
;;       )
