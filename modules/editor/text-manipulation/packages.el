;; -*- no-byte-compile: t; -*-
;;; util/text/packages.el
(package! rotate-text)
(package! evil-states-plus :recipe (:host github :repo "jgrey4296/evil-states-plus" :includes (insert-plus-state other-chars-state)))
(package! embrace)

;; Spelling
(package! spell-fu)
(package! flyspell-correct)
(package! flyspell-correct-ivy)
(package! flyspell-lazy)
(package! writegood-mode)
(package! accent)

;; Formatting
(package! format-all)
(package! dtrt-indent)
(package! adaptive-wrap)

;; (package! objed)
