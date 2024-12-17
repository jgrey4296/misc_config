;;; +spec-defs.el -*- lexical-binding: t; -*-

(spec-handling-new! auto-modes
                    "Handler to control automodes"
                    :target auto-mode-alist
                    :loop 'append
                    :struct '(key  val:list\[(regex . mode)\])
                    val
                    )
