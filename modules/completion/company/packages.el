;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! cl-lib)
(package! company )
(package! company-dict )
(when (featurep! +childframe)
  (package! company-box ))
