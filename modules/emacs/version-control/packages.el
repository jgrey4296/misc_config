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

(package! vc :built-in t)
(package! vc-annotate :built-in t)
(package! smerge-mode :built-in t)

(package! browse-at-remote)
(package! git-commit)
(package! git-timemachine :recipe (:host github :repo "emacsmirror/git-timemachine"))
(package! git-modes)
