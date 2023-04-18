;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el

(package! avy)
(package! link-hint)

(unless (modulep! :editor evil)
  (package! expand-region))

(package! spec-handling :recipe `(:local-repo ,(expand-file-name "packages/misc/spec-handling" doom-user-dir)))
