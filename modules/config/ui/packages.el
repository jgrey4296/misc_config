;; -*- no-byte-compile: t; -*-
;;; emacs/jg-ui/packages.el


(package! anzu)
(package! auto-highlight-symbol)
(package! doom-modeline)
(package! font-lock+ :recipe (:host github :repo "emacsmirror/font-lock-plus"))
(package! highlight-indent-guides)
(package! highlight-parentheses)
(package! hilit-chg)
(package! hl-todo)
(package! palette-mode :recipe `(:local-repo ,(expand-file-name "packages/major-modes/palette-mode" doom-user-dir)))
(package! rainbow-delimiters)
(package! rainbow-mode)
(package! smartparens)
(package! transient)
(package! transient-macros :recipe (:host github :repo "jgrey4296/transient-macros"))
(package! visual-fill-column)
(package! whitespace)
(package! treemacs)
(package! paren)
(package! hide-mode-line)
