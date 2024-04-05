;;; +spec-defs.el -*- lexical-binding: t; -*-


;; (spec-handling-new! quickrun-files quickrun-file-alist)
;; (spec-handling-new! quickrun-modes quickrun--major-mode-alist)
;; (spec-handling-new! quickrun-files quickrun--language-alist)

(spec-handling-new! repl +eval-repls
                    :loop 'collect
                    :doc "Registers repl handlers"
                    :struct '(:modes list :start fn :send fn :persist bool :run fn)
                    (cons (or (plist-get val :name) key) val)
                    )

(spec-handling-new! compile-commands counsel-compile-local-builds :loop 'append
                    val
                    )
