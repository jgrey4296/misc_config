;;; lang/haskell/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(defer-load! jg-bindings-total "+bindings")
(after! projectile
  (add-to-list 'projectile-project-root-files "stack.yaml"))

;;
;;; Common packages

(use-package! haskell-mode
  :commands (haskell-mode ghci-script-mode gch-core-mode)
  :config
  (add-hook! 'haskell-mode-hook
             #'haskell-collapse-mode ; support folding haskell code blocks
             #'interactive-haskell-mode
             #'tree-sitter!
             )

  (add-to-list 'completion-ignored-extensions ".hi")

  (define-advice +haskell/open-repl (:before (&optional arg)
                                             +jg-haskell-repl-require)
    "The haskell repl start of doom doesn't require haskell to start with"
    (require 'haskell)
    )
)

(use-package! lsp-haskell
  :after (haskell-mode lsp-mode)
  :defer t
  :init
  :config
  ;; Does some strange indentation if it pastes in the snippet
  (setq-hook! 'haskell-mode-hook yas-indent-line 'fixed)
  )
