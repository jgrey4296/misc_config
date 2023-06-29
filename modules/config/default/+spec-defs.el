;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new! automodes auto-mode-alist :loop 'append
                    :doc "Handler to control automodes"
                    :struct '(key  val:list\[(regex . mode)\])
                    val
                    )

(spec-handling-add! automodes
                    '(default
                       ("\\.rs\\'" . rustic-mode)
                       ("\\.py\\'" . python-mode)
                       ("\\.el\\'" . emacs-lisp-mode)
                       ("\\.org\\'" . org-mode)
                       )
                    )
