;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company)
(package! company-dict)
(package! jg-company :recipe (:host github :repo "jgrey4296/misc-modes" :files ("minor-modes/jg-company/*.el")))
