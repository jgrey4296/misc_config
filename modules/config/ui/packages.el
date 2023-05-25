;; -*- no-byte-compile: t; -*-
;;; emacs/jg-ui/packages.el

(package! anzu)
(package! auto-highlight-symbol)
(package! centered-cursor-mode)
(package! doom-modeline)
(package! highlight-indent-guides)
(package! highlight-parentheses)
(package! hilit-chg)
(package! hl-todo)
(package! palette-mode :recipe `(:local-repo ,(expand-file-name "packages/major-modes/palette-mode" doom-user-dir)))
(package! rainbow-delimiters)
(package! rainbow-mode)
(package! smartparens)
(package! transient)
(package! visual-fill-column)
(package! whitespace)
