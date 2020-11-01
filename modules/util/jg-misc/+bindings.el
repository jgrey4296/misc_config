;;; util/jg-misc/+bindings.el -*- lexical-binding: t; -*-

(map! :leader
      (:prefix "x"
      :n :desc "Uniqify" "l u" #'+jg-misc-uniquify))
