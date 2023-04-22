;; -*- no-byte-compile: t; -*-
;;; lang/graphql/packages.el

(package! graphql-mode)
(package! graphql-doc)
(unless (modulep! +lsp) (package! company-graphql :recipe (:host github :repo "thaenalpha/company-graphql")))
