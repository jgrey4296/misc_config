;; -*- no-byte-compile: t; -*-
;;; editor/snippets/packages.el

(package! yasnippet)
(package! auto-yasnippet)
(package! doom-snippets :recipe (:host github :repo "doomemacs/snippets" :files (:defaults "*")))
