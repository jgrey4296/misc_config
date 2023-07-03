;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new! auto-modes auto-mode-alist :loop 'append
                    :doc "Handler to control automodes"
                    :struct '(key  val:list\[(regex . mode)\])
                    val
                    )
