;;; +bindings.el -*- lexical-binding: t; -*-

  (map! :map flycheck-error-list-mode-map
        :n "C-n"    #'flycheck-error-list-next-error
        :n "C-p"    #'flycheck-error-list-previous-error
        :n "j"      #'flycheck-error-list-next-error
        :n "k"      #'flycheck-error-list-previous-error
        :n "RET"    #'flycheck-error-list-goto-error
        :n [return] #'flycheck-error-list-goto-error)

(map! :map tabulated-list-mode-map
      "?" #'+jg-checkers-column-format
      :n "w" #'tabulated-list-next-column
      :n "b" #'tabulated-list-previous-column
      )
