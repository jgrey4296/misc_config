;; -*- no-byte-compile: t; -*-
;;; emacs/jg-vc/packages.el

(package! cond-let :pin "288b7d36563223ebaf64cb220a3b270bdffb63f1") ; 0.2.0
(package! magit :pin "4800ace210d5e...")
(package! magit-todos )
(package! git-cliff)
(package! vdiff-magit)
(package! git-timemachine :recipe (:host github :repo "emacsmirror/git-timemachine"))
(package! git-modes)
(package! git-gutter-fringe)
(package! treemacs-magit)
(package! ghub :recipe (:host github :repo "magit/ghub"))
(package! forge )

(package! evil-states-plus :recipe (:host github :repo "jgrey4296/evil-states-plus" :includes (conflict-merge-state)))
(package! code-review :recipe (:files ("graphql" "code-review*.el")))

(package! conventional-changelog)
(package! markdown-changelog)

(package! fringe :built-in t)
(package! vi-tilde-fringe)
(package! diff-hl)
(package! vc :built-in t)
(package! vc-annotate :built-in t)
(package! smerge-mode :built-in t)
(package! browse-at-remote)
