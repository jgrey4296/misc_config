;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new! fold evil-fold-list :sorted t :loop 'collect
                    :doc "Registers fold handlers"
                    :struct '(:modes list :priority int :triggers
                              (:delete fn :open-all fn :close-all-fn :toggle fn :open fn :open-rec fn :close fn))
                    (append (list (* -1 (or (plist-get val :priority) 0)))
                            (list (ensure-list (plist-get val :modes)))
                            (plist-get val :triggers)
                            )
                    )

(spec-handling-new! hideshow hs-special-modes-alist :loop 'append
                    val
                    )
