;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el

(package! avy)
(package! link-hint)

(unless (modulep! :editor evil)
  (package! expand-region))
