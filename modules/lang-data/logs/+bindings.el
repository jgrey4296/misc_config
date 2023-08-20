;;; +bindings.el -*- lexical-binding: t; -*-


(map! :map jg-log-mode-map
      :n "|" #'jg-log-mode-filter
      :n "] ]" #'jg-log-forward-section
      :n "[ [" #'jg-log-backward-section
      )
