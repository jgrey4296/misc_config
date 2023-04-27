;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el
(package! avy )
(package! cl-lib :built-in t)
(package! iedit)
(package! link-hint )
(package! expand-region)
(package! faster-whichkey :recipe `(:local-repo ,(expand-file-name "packages/misc/faster-whichkey" doom-user-dir)))
(package! which-key)
