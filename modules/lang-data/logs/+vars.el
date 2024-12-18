;;; +vars.el -*- lexical-binding: t; -*-

(speckler-add! auto-modes ()
  '(logs
    ("log\\..+\\'" . fundamental-mode)
    ("\\.log$"     . fundamental-mode)
    )
  )
