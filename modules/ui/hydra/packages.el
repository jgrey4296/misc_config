;; -*- no-byte-compile: t; -*-
;;; ui/hydra/packages.el

(package! hydra)
(package! hydra-utils :recipe `(:local-repo ,(expand-file-name "packages/misc/hydra-utils" doom-user-dir)))
