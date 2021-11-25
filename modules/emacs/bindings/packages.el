;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el
(package! avy )
(package! cl-lib :built-in t)
(package! ibuffer)
(package! iedit)
(package! link-hint )
(package! expand-region)
(package! which-mod :recipe (:local-repo "~/.doom.d/packages/which-mod"))
(package! general-mod :recipe (:local-repo "~/.doom.d/packages/general-mod"))
