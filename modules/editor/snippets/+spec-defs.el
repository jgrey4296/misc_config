;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new! file-templates +file-templates-alist t append
                    (cl-loop for rule in val
                             for priority = (* -1 (or (plist-get rule :priority) 0))
                             for clean    = (cl-loop for (k v) on rule by #'cddr
                                                     unless (eq k :priority)
                                                     if k collect k
                                                     if v collect v)
                             collect (cons priority clean)
                             )
                    )
