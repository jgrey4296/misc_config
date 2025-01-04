;;; +spec-defs.el -*- lexical-binding: t; -*-

(speckler-new-hook! imenu (key val)
  "Register imenu generic expressions"
  :struct '(strName :append? regexp)
  :override t
  (setq-local imenu-generic-expression
              (if (eq (car val) :append)
                  (append (cdr val) imenu-generic-expression)
                val)
              )
  )
