;;; +vars.el -*- lexical-binding: t; -*-

(speckler-add! evil-ex ()
  '(support
    ("cc"          . #'evil-goto-error)
    ("cfir[st]"    . #'first-error)
    ("cr[ewind]"   . #'first-error)
    ("cn[ext]"     . #'next-error)
    ("cp[revious]" . #'previous-error)
    ("com[pile]"   . #'+evil:compile)
    )
  )
