;;; lang/ocaml/config.el -*- lexical-binding: t; -*-

;;
;;; Packages
(local-load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(use-package! utop
    :after tuareg
    :hook (tuareg-mode-local-vars . +ocaml-init-utop-h)
    )

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

(use-package! flycheck-ocaml
  :after merlin
  :hook (merlin-mode . +ocaml-init-flycheck-h)
  )

(use-package! merlin-eldoc
  :after merlin
  :hook (merlin-mode . merlin-eldoc-setup)
  )

(use-package! merlin-iedit
  :after merlin
  )

(use-package! merlin-imenu
  :after merlin
  :hook (merlin-mode . merlin-use-merlin-imenu)
  )

(use-package! ocp-indent
  :after tuareg
  ;; must be careful to always defer this, it has autoloads that adds hooks
  ;; which we do not want if the executable can't be found
  :hook (tuareg-mode-local-vars . +ocaml-init-ocp-indent-h)
  )

(use-package! ocamlformat
  :commands ocamlformat
  :hook (tuareg-mode-local-vars . +ocaml-init-ocamlformat-h)

  )

(use-package! sml-mode
  :commands sml-mode
  :config
  ;; don't auto-close apostrophes (type 'a = foo) and backticks (`Foo)
  (sp-with-modes 'sml-mode
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" nil :actions nil))

  )

(use-package! company-mlton
  :after sml-mode
  :hook (sml-mode . company-mlton-init)
  )
