;; -*- no-byte-compile: t; -*-
;;; emacs/jg-ui/packages.el

(package! auto-highlight-symbol)
(package! centered-cursor-mode)
(package! evil-visual-mark-mode)
(package! whitespace)
(package! swiper)
(package! window-ring-minor-mode :recipe `(:local-repo ,(expand-file-name "packages/minor-modes/window-ring-minor-mode" doom-user-dir)))
(package! popup)
(package! palette-mode :recipe `(:local-repo ,(expand-file-name "packages/major-modes/palette-mode" doom-user-dir)))
(package! smartparens)
(package! paren-state :recipe `(:local-repo ,(expand-file-name "packages/states/paren-state" doom-user-dir)))

(package! ibuffer)