;;; +bindings.el -*- lexical-binding: t; -*-


(map! :leader
      :after flycheck
      :desc "Flycheck" "!" flycheck-command-map
      :prefix "c"
      :desc "Flycheck" "!" flycheck-command-map
      )

(map! :map flycheck-error-list-mode-map
      :n "C-n"    #'flycheck-error-list-next-error
      :n "C-p"    #'flycheck-error-list-previous-error
      :n "j"      #'flycheck-error-list-next-error
      :n "k"      #'flycheck-error-list-previous-error
      :n "RET"    #'flycheck-error-list-goto-error
      :n [return] #'flycheck-error-list-goto-error
      :n "," nil
      :n "," #'tabulated-list-sort
      :n "{" #'tabulated-list-narrow-current-column
      :n "}" #'tabulated-list-widen-current-column
      )

(map! :map tabulated-list-mode-map
      "?" #'+jg-checkers-column-format
      :n "w" #'tabulated-list-next-column
      :n "b" #'tabulated-list-previous-column
      )
