;;; lang/ocaml/config.el -*- lexical-binding: t; -*-

;;
;;; Packages
(local-load! "+extra-config")

(defer-load! jg-bindings-total "+bindings")

(use-package! tuareg
  :commands tuareg-mode
  :config
  ;; harmless if `prettify-symbols-mode' isn't active
  (setq tuareg-prettify-symbols-full t)

  ;; Use opam to set environment
  (setq tuareg-opam-insinuate t)
  (tuareg-opam-update-env (tuareg-opam-current-compiler))

  (setq-hook! 'tuareg-mode-hook
    comment-line-break-function #'+ocaml/comment-indent-new-line)
  (add-hook! 'tuareg-mode-hook #'tree-sitter!)
  )

(use-package! merlin
  :after tuareg
  :hook (tuareg-mode-local-vars . +ocaml-init-merlin-h)
  :config
  (setq merlin-completion-with-doc t)
  )

(use-package! sml-mode
  :commands sml-mode
  :config
  ;; don't auto-close apostrophes (type 'a = foo) and backticks (`Foo)
  (sp-with-modes 'sml-mode
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" nil :actions nil))

  )

(use-package! ocaml-ts-mode

  )

;; (use-package! dune-mode)

(speckler-add! tree-sitter-lang ()
  '(ocaml-mode         . ocaml)
  '(ocaml-ts-mode      . ocaml)
  )
(speckler-add! treesit-source ()
  '(ocaml         "git@github.com:tree-sitter/tree-sitter-ocaml.git")
  )
(speckler-add! company ()
  '(tuareg-mode merlin-company-backend)
  '(sml-mode company-mlton-grouped-backend)
  )
(speckler-add! doc-lookup ()
  '(tuareg-mode :async t
    :definition merlin-locate
    :references merlin-occurrences
    :documentation merlin-document
    )
  )
(speckler-add! popup ()
  '(ml-langs
    ("^\\*utop\\*" :quit nil)
    )
  )
(speckler-add! auto-modes ()
  '(ml-langs
    ("\\.mlg$" . tuareg-mode)
    ("\\.s\\(?:ml\\|ig\\)\\'" . sml-mode)
    )
  )
(speckler-add! repl ()
  '(sml-mode :start run-sml)
  '(tuareg-mode
    :start utop
    :send utop-eval-region
    )
  )
(speckler-add! file-templates ()
  '(ocaml
    ("dune\\'" :trigger "__dune" :mode ocaml-mode)
    ("\\.ml\\;" :trigger "__" :mode ocaml-mode)
    )
  )
