;; -*- no-byte-compile: t; -*-
;;; editor/fold/packages.el

(package! hideshow :built-in t)

(package! vimish-fold )
(when (featurep! :editor evil)
  (package! evil-vimish-fold ))
(package! cl-lib)
(package! origami)
