;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new! python-env env-handling-registered
                    :loop 'append
                    val
                    ;; todo (apply #'make-env-handler val)
                    )
