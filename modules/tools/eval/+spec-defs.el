;;; +spec-defs.el -*- lexical-binding: t; -*-


;; (spec-handling-new! quickrun-files quickrun-file-alist)
;; (spec-handling-new! quickrun-modes quickrun--major-mode-alist)
;; (spec-handling-new! quickrun-files quickrun--language-alist)

(spec-handling-new! eval +eval-repls
                    :loop 'collect
                    :doc "Registers eval handlers"
                    :struct '(:modes list :start fn :send fn :persist bool)
                    (cons (or (plist-get val :name) key) val)
                    )
