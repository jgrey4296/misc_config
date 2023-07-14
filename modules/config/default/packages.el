;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el

(package! f)
(package! link-hint)

(unless (modulep! :editor evil)
  (package! expand-region))

(package! spec-handling :recipe (:host github :repo "jgrey4296/spec-handling"))
