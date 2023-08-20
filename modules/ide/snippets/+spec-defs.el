;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new! file-templates +file-templates-alist :sorted t :loop 'append
                    (cl-loop for rule in val
                             for priority = (* -1 (or (plist-get rule :priority) 0))
                             for clean    = (cl-loop for (k v) on rule by #'cddr
                                                     unless (eq k :priority)
                                                     if k collect k
                                                     if v collect v)
                             collect (cons priority clean)
                             )
                    )

(spec-handling-new! yas-extra nil :loop 'hook
                    :doc "activate minor modes for yasnippet"
                    :struct '(extras)
                    (dolist (mode (ensure-list val))
                      (yas-activate-extra-mode mode)
                      )
                    )
