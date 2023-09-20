;; -*- no-byte-compile: t; -*-
;;; editor/snippets/packages.el

(package! yasnippet)
(package! auto-yasnippet)
(package! doom-snippets :recipe (:host github :repo "doomemacs/snippets" :files (:defaults "*")))
(package! yasnippet-snippets)
(package! academic-phrases)
(package! license-templates)
(package! lorem-ipsum)

;; (package! dabbrev :type 'built-in)
;; (package! abbrev :type 'built-in)
