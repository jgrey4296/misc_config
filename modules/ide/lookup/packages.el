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
(package! dash-docs)
(package! counsel-dash)
(package! browse-select :recipe (:host github :repo "jgrey4296/misc-modes" :files ("minor-modes/browse-select/*.el")))
(package! lookup-regular :recipe (:host github :repo "jgrey4296/misc-modes" :files ("minor-modes/lookup-regular/*.el")))
(package! helm-wordnet)
(package! xref)
