;;; +repl.el -*- lexical-binding: t; -*-

(define-advice +haskell/open-repl (:before (&optional arg)
                                   +jg-haskell-repl-require)
  "The haskell repl start of doom doesn't require haskell to start with"
  (require 'haskell)
  )
