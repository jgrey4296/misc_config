;; -*- no-byte-compile: t; -*-
;;; tools/lookup/packages.el

(package! xref)
(package! ivy-xref)

;; For dictionary and online lookup
(package! request)

(if IS-MAC
    (package! osx-dictionary)
  )

(package! wordnut)
(package! browse-url)
