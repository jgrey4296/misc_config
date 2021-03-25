;;; lang/jg-python/+bindings.el -*- lexical-binding: t; -*-

(map! :after python
      :map python-mode-map
      :n "z d" #'+jg-personal-toggle-all-defs
      :n "z D" #'+jg-personal-close-class-defs
      :v "i f" #'+jg-python-select-defun
      :v "i F" #'+jg-python-select-class
      :leader
      (:prefix ("i" . "Insert")
        :desc "Insert Breakpoint" "d" #'+jg-personal-python-toggle-breakpoint
       )
      :localleader
      :desc "Sort defs" "S" #'+jg-python-sort-class-methods
      )
