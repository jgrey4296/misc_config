;; -*- no-byte-compile: t; -*-
;;; tools/lookup/packages.el

(package! xref)
(package! ivy-xref)

;; For dictionary and online lookup
(package! request)

(if (eq system-type 'darwin)
    (package! osx-dictionary)
  )

(package! browse-url :built-in t)
(package! dash-docs)
(package! counsel-dash)

(package! wordnut)
(package! helm-wordnet)
(package! synosaurus)
(package! librarian :recipe (:host github :repo "jgrey4296/librarian"))
