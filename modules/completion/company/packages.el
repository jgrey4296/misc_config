;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company)
(package! company-dict)
(package! jg-company :type 'local :recipe `(:local-repo ,(expand-file-name "packages/misc/jg-company" doom-user-dir)))
