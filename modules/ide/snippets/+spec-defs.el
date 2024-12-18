;;; +spec-defs.el -*- lexical-binding: t; -*-

(speckler-new! file-templates (key val)
  "Register File Templates"
  :target +file-templates-alist
  :sorted t
  :loop 'append
  :struct '(key . (list (pattern :trigger pattern :mode mode)))
  (cl-loop for rule in val
           for priority = (* -1 (or (plist-get rule :priority) 0))
           for clean    = (cl-loop for (k v) on rule by #'cddr
                                   unless (eq k :priority)
                                   if k collect k
                                   if v collect v)
           collect (cons priority clean)
           )
  )

(speckler-new-hook! yas-extra (key val)
  "activate minor modes for yasnippet"
  :struct '(extras)
  (dolist (mode (ensure-list val))
    (yas-activate-extra-mode mode)
    )
  )
