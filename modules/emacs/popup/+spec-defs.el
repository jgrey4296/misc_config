;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new! popup display-buffer-alist :sorted t :loop 'append
                    (cl-loop for rule in val
                             collect
                             (cons (* -1 (or (plist-get (cdr rule) :priority) 0))
                                   (+popup-make-rule (car rule) (cdr rule))
                                   )
                             )
                    )
