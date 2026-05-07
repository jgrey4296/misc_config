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

(package! librarian)

(package! librarian-tag-helm)
(package! librarian-tag-ivy)

(package! rawtag-mode)
(package! subfile-mode)

(package! dumb-jump)

(package! spell-fu)
(package! flyspell-correct)
(package! flyspell-correct-ivy)
(package! flyspell-lazy)
