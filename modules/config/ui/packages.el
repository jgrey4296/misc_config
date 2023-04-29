;; -*- no-byte-compile: t; -*-
;;; emacs/jg-ui/packages.el

(package! auto-highlight-symbol)
(package! centered-cursor-mode)
(package! evil-visual-mark-mode)
(package! whitespace)
(package! swiper)
(package! smartparens)
(package! palette-mode           :recipe `(:local-repo ,(expand-file-name "packages/major-modes/palette-mode" doom-user-dir)))
(package! doom-modeline)
(package! anzu)
(package! evil-anzu)
