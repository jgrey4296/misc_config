;;; lang/emacs-lisp/+bindings.el -*- lexical-binding: t; -*-
(message "Loading modules/lang/jg-lisp/+bindings.el")

(map! :map emacs-lisp-mode-map
      :localleader
      :desc "Sort Defuns" "S" #'+jg-lisp-sort-defuns
      )
