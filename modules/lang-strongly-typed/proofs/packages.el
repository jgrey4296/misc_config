;; -*- no-byte-compile: t; -*-
;;; lang/coq/packages.el

(package! proof-general
  ;; REVIEW: Remove when ProofGeneral/PG#771 is fixed. Also see #8169.
  :recipe (:build (:not autoloads)))
(package! company-coq)
(package! idris-mode :pin "c96f45d1b8fad193f09fb6139da17092003b5e74")

(package! agda-input :recipe (:host github :repo "agda/agda" :files ("src/data/emacs-mode/agda-input.el") :nonrecursive t) :pin "bb7603d19781e4da2dc702a5a1611fd59e5325f2")
(package! agda2-mode :recipe (:host github :repo "agda/agda" :files ("src/data/emacs-mode/*.el" (:exclude "agda-input.el")) :nonrecursive t) :pin "bb7603d19781e4da2dc702a5a1611fd59e5325f2")

(package! fstar-mode :pin "ab0697b9474f36942a12a4b2a75251c247c18e9e")
(package! lean-mode)
