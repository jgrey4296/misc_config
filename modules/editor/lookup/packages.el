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
(package! browse-select :recipe `(:local-repo ,(expand-file-name "packages/misc/browse-select" doom-user-dir)))
(package! lookup-regular :recipe `(:local-repo ,(expand-file-name "packages/minor-modes/lookup-regular" doom-user-dir)))
(package! helm-wordnet)
(package! xref)
