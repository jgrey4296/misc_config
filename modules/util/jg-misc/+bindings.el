;;; util/jg-misc/+bindings.el -*- lexical-binding: t; -*-

(map! :leader
      (:prefix "x"
      :desc "Uniqify" "l u" #'+jg-misc-uniquify)
      (:prefix "b"
       :desc "Undo-Tree" "u" #'+jg-misc-undo-tree)
    )
