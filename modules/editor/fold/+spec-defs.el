;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new! fold
                    "Registers fold handlers"
                    :target evil-fold-list
                    :sorted t
                    :loop 'collect
                    :struct '(:modes list :priority int :triggers
                              (:delete fn :open-all fn :close-all-fn :toggle fn :open fn :open-rec fn :close fn))
                    (append (list (* -1 (or (plist-get val :priority) 0)))
                            (list (ensure-list (plist-get val :modes)))
                            (cl-loop for (kwd . fn) in (map-pairs (plist-get val :triggers))
                                     collect kwd
                                     collect (spec-handling-unquote! fn))
                            )
                    )

(spec-handling-new! hideshow
                    "Set hide show special modes"
                    :target hs-special-modes-alist
                    :loop 'append
                    val
                    )
