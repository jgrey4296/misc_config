;; -*- no-byte-compile: t; -*-
;;; emacs/jg-vc/packages.el

(package! smerge-mode :built-in t)

(package! magit)
(package! forge )
(package! code-review :recipe (:files ("graphql" "code-review*.el")))
(package! magit-todos )
