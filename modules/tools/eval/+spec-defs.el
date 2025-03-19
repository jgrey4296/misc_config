;;; +spec-defs.el -*- lexical-binding: t; -*-

(defvar +eval-repls nil "Stores handlers to run repls")

(defvar +eval-handlers nil "Stores handlers to eval buffers")

(cl-defstruct (repl-handler)
  "Specifies how to start and run repls"
  (modes   nil :type list)
  (persist nil :type bool)
  (start   nil :type lambda)
  (send    nil :type lambda)
  (run     nil :type lambda)
  )

(cl-defstruct (eval-handler)
  "Specifies how to eval regions/buffers"
  (modes nil :type list)
  (fn    nil :type lambda :doc (lambda start end))
  (indirect nil :Type lambda)
  )

(speckler-new! repl (key val)
  "Registers repl handlers"
  :target +eval-repls
  :loop 'collect
  :struct '(or repl-handler (:modes list :start fn :send fn :persist bool :run fn))
  (cond ((repl-handler-p val)
         (cons key val))
        (t (cons key (apply #'make-repl-handler :modes key val)))
        )
  )

(speckler-new! eval (key val)
  "Registers Eval Handlers"
  :target +eval-handlers
  :loop 'collect
  :struct '(or eval-handler (mode :region))
  (cond ((eval-handler-p val)
         (cons key val))
        (t (cons key (apply #'make-eval-handler :modes key val)))
        )
  )

(speckler-new! compile-commands (key fns)
  "Register commands for trying to compile projects. Functions return strings of commands"
  :target counsel-compile-local-builds
  :loop 'append
  (mapcar #'upfun! (ensure-list fns))
  )
