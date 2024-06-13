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


(spec-handling-new! repl +eval-repls
                    :loop 'collect
                    :doc "Registers repl handlers"
                    :struct '(or repl-handler (:modes list :start fn :send fn :persist bool :run fn))
                    (cond ((repl-handler-p val)
                           (cons key val))
                          (t (cons key (apply #'make-repl-handler :modes key val)))
                          )
                    )

(spec-handling-new! eval +eval-handlers
                    :loop 'collect
                    :doc "Registers Eval Handlers"
                    :struct '(or eval-handler (mode :region))
                    (cond ((eval-handler-p val)
                           (cons key val))
                          (t (cons key (apply #'make-eval-handler :modes key val)))
                          )
                    )

(spec-handling-new! compile-commands counsel-compile-local-builds :loop 'append
                    val
                    )


;; (spec-handling-new! quickrun-files quickrun-file-alist)
;; (spec-handling-new! quickrun-modes quickrun--major-mode-alist)
;; (spec-handling-new! quickrun-files quickrun--language-alist)
