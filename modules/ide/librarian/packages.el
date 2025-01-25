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

(package! librarian-tag-helm :recipe (:host github :repo "jgrey4296/misc-modes" :files ("minor-modes/librarian-tag-helm/*.el") :local-repo "misc-modes"))
(package! librarian-tag-ivy  :recipe (:host github :repo "jgrey4296/misc-modes" :files ("minor-modes/librarian-tag-ivy/*.el") :local-repo "misc-modes"))

(package! subfile-mode           :recipe (:host github :repo "jgrey4296/misc-modes" :files ("major-modes/subfile-mode/*.el") :local-repo "misc-modes"))
