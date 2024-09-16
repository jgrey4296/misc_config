;;; lang/ocaml/config.el -*- lexical-binding: t; -*-

;;
;;; Packages
(local-load! "+vars")
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
