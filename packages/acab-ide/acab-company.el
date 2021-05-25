;; -*- mode: elisp; lexical-binding: t; -*-

;; TODO context aware company completion of rule names, layers,
;; operators etc
(require 'cl-lib)
(require 'company)


(defun acab-company/backend (cmd &rest args)
  (cl-case cmd
    (init            nil)
    (prefix          nil)
    (candidates      nil)
    (sorted          t)
    (duplicates      t)
    (no-cache        nil)
    (ignore-case     t)
    (annotation      nil)
    (meta            nil)
    (location        nil)
    (post-completion nil)
    (require-match   nil)
    (t               nil)
    )
  )
