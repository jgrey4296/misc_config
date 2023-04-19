;; -*- no-byte-compile: t; -*-
;;; completion/company/packages.el

(package! company)
(package! company-dict)
(package! jg-company-minor-mode :type 'local :recipe `(:local-repo ,(expand-file-name "packages/minor-modes/jg-company-minor-mode" doom-user-dir)))
