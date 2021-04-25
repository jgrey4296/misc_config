;;; lang/emacs-lisp/+bindings.el -*- lexical-binding: t; -*-

(map! :map emacs-lisp-mode-map
      :localleader
      :desc "Sort Defuns" "S" #'+jg-lisp-sort-defuns
      )
