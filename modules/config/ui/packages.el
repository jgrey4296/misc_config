;; -*- no-byte-compile: t; -*-
;;; emacs/jg-ui/packages.el

(package! auto-highlight-symbol)
(package! centered-cursor-mode)
(package! evil-visual-mark-mode)
(package! whitespace)
(package! swiper)
(package! smartparens)
(package! window-ring-minor-mode :recipe `(:local-repo ,(expand-file-name "packages/minor-modes/window-ring-minor-mode" doom-user-dir)))
(package! palette-mode           :recipe `(:local-repo ,(expand-file-name "packages/major-modes/palette-mode" doom-user-dir)))
(package! paren-state            :recipe `(:local-repo ,(expand-file-name "packages/states/paren-state" doom-user-dir)))
