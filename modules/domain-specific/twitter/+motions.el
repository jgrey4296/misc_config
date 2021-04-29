;;; domain-specific/twitter/+motions.el -*- lexical-binding: t; -*-


(evil-define-operator +jg-twitter-tweet-operator (beg end)
  :type inclusive
  (+jg-twitter-tweet-with-input (buffer-substring-no-properties beg end))
  )


(map! :map jg-binding-operator-map
      :prefix "s"
      :desc "Tweet Text" "T" #'+jg-twitter-tweet-operator)
