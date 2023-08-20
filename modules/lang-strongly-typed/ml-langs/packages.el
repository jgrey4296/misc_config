;; -*- no-byte-compile: t; -*-
;;; lang/ocaml/packages.el

(package! tuareg)

(unless (modulep! +lsp)
  (package! merlin)
  (package! merlin-eldoc)
  (package! merlin-company)
  (when (modulep! :checkers syntax)
    (package! flycheck-ocaml)))

(package! ocp-indent)
(when (modulep! :tools eval) (package! utop))
(when (modulep! :editor format) (package! ocamlformat :recipe (:host github :repo "ocaml-ppx/ocamlformat" :files ("emacs/*.el"))))
(package! dune :recipe (:host github :repo "ocaml/dune" :files ("editor-integration/emacs/*.el")))
(package! sml-mode)
(package! company-mlton :recipe (:host github :repo "MatthewFluet/company-mlton" :files ("*.el" "*.basis")))
