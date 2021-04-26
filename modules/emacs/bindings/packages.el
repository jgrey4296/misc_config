;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el
(package! cl-lib)

(package! avy )
(package! link-hint )
(package! iedit)
(unless (featurep! :editor evil)
  (package! expand-region ))

(package! mouse :disable t)
(package! ibuffer)
(package! general)
