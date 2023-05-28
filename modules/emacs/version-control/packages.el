;; -*- no-byte-compile: t; -*-
;;; emacs/jg-vc/packages.el

(package! smerge-mode :built-in t)

(package! magit)
(package! forge )
(package! code-review :recipe (:files ("graphql" "code-review*.el")))
(package! magit-todos )
(package! vdiff-magit)
(package! vi-tilde-fringe)

(if (modulep! +diff-hl)
    (package! diff-hl)
  (package! git-gutter-fringe))
