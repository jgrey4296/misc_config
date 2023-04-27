;; -*- no-byte-compile: t; -*-
;;; lang/sh/packages.el

(when (modulep! :completion company)
  (package! company-shell))
