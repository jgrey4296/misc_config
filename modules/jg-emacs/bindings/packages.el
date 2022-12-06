;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el
(package! avy )
(package! cl-lib :built-in t)
(package! ibuffer)
(package! iedit)
(package! link-hint )
(package! expand-region)
(package! which-mod :recipe `(:local-repo ,(expand-file-name "packages/which-mod" doom-user-dir)))
(package! general-mod :recipe `(:local-repo ,(expand-file-name "packages/general-mod" doom-user-dir)))
(package! which-key)
