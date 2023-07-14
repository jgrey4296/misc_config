;;; lang/ocaml/config.el -*- lexical-binding: t; -*-

;;
;;; Packages
(defer-load! "+vars")

(use-package! utop
    :when (modulep! :tools eval)
    :after tuareg
    :hook (tuareg-mode-local-vars . +ocaml-init-utop-h)
    :init
    (spec-handling-add! eval
                        `(tuareg-mode
                          :start ,#'utop
                          :send ,#'utop-eval-region
                          )
                        )
    )

(after! tuareg
  ;; harmless if `prettify-symbols-mode' isn't active
  (setq tuareg-prettify-symbols-full t)

  ;; Use opam to set environment
  (setq tuareg-opam-insinuate t)
  (tuareg-opam-update-env (tuareg-opam-current-compiler))

  (setq-hook! 'tuareg-mode-hook
    comment-line-break-function #'+ocaml/comment-indent-new-line)

  (map! :localleader
        :map tuareg-mode-map
        "a" #'tuareg-find-alternate-file)

  (add-hook! 'tuareg-mode-hook #'tree-sitter!)
  )

(use-package! merlin
  :defer t
  :hook (tuareg-mode-local-vars . +ocaml-init-merlin-h)
  :config
  (setq merlin-completion-with-doc t)

  (map! :localleader
        :map tuareg-mode-map
        "t" #'merlin-type-enclosing)
  )

(use-package! flycheck-ocaml
  :when (modulep! :checkers syntax)
  :after merlin
  :hook (merlin-mode . +ocaml-init-flycheck-h)
  )

(use-package! merlin-eldoc
  :after merlin
  :hook (merlin-mode . merlin-eldoc-setup)
  )

(use-package! merlin-iedit
  :after merlin
  :when (modulep! :editor multiple-cursors)
  :defer t
  :init
  (map! :map tuareg-mode-map
        :v "R" #'merlin-iedit-occurrences)
  )

(use-package! merlin-imenu
  :when (modulep! :emacs imenu)
  :after merlin
  :hook (merlin-mode . merlin-use-merlin-imenu)
  )

(use-package! ocp-indent
  :defer t
  ;; must be careful to always defer this, it has autoloads that adds hooks
  ;; which we do not want if the executable can't be found
  :hook (tuareg-mode-local-vars . +ocaml-init-ocp-indent-h)
  )

(use-package! ocamlformat
  :when (modulep! :editor format)
  :commands ocamlformat
  :hook (tuareg-mode-local-vars . +ocaml-init-ocamlformat-h)

  )


(use-package! sml-mode
  :defer t
  :config

  ;; don't auto-close apostrophes (type 'a = foo) and backticks (`Foo)
  (sp-with-modes 'sml-mode
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" nil :actions nil))

  (map! :map sml-mode-map
        :i "RET"   #'reindent-then-newline-and-indent
        :i "S-SPC" #'sml-electric-space
        :i "|"     #'sml-electric-pipe
        :localleader
        :desc "Run SML" "'" #'run-sml
        :prefix ("e" . "eval")
        :desc "Run buffer"                  "b" #'sml-prog-proc-send-buffer
        :desc "Run the paragraph"           "f" #'sml-send-function
        :desc "Run region"                  "r" #'sml-prog-proc-send-region)
  )

(use-package! company-mlton
  :defer t
  :when (modulep! :completion company)
  :hook (sml-mode . company-mlton-init)
  :config
  )
