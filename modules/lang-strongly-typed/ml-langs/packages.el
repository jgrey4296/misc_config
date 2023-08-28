;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg)

(package! merlin)
(package! merlin-eldoc)
(package! merlin-company)
(package! flycheck-ocaml)

(package! ocp-indent)
(package! utop)
(package! ocamlformat :recipe (:host github :repo "ocaml-ppx/ocamlformat" :files ("emacs/*.el")))
(package! dune :recipe (:host github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el")))
(package! sml-mode)
(package! company-mlton :recipe (:host github :repo "MatthewFluet/company-mlton" :files ("*.el" "*.basis")))
