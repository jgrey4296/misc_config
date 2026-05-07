;; -*- no-byte-compile: t; -*-
;;; lang/coq/packages.el

(package! proof-general :recipe (:build (:not autoloads)))
(package! company-coq)
(package! idris-mode)

(package! agda-input)
(package! agda2-mode)

(package! fstar-mode)
(package! lean-mode)
