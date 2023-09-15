;; -*- no-byte-compile: t; -*-
;;; tools/lookup/packages.el

(package! xref)
(package! ivy-xref)

;; For dictionary and online lookup
(package! request)

(if (eq system-type 'darwin)
    (package! osx-dictionary)
  )

(package! browse-url)
(package! dash-docs)
(package! counsel-dash)
(package! xref)

(package! wordnut)
(package! helm-wordnet)
;; (package! powerthesaurus)
(package! synosaurus)
(package! librarian :recipe (:host github :repo "jgrey4296/librarian"))
