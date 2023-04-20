;;; lang/haskell/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(after! jg-bindings-total
  (load! "+bindings"))
(after! projectile
  (add-to-list 'projectile-project-root-files "stack.yaml"))

;;
;;; Common packages

(use-package! haskell-mode
  :defer t
  :config
  (add-hook! 'haskell-mode-hook
             #'haskell-collapse-mode ; support folding haskell code blocks
             #'interactive-haskell-mode)

  (when (modulep! +tree-sitter)
    (add-hook 'haskell-mode-local-vars-hook #'tree-sitter! 'append))

  (add-to-list 'completion-ignored-extensions ".hi")

)

(use-package! lsp-haskell
  :when (modulep! +lsp)
  :defer t
  :init
  (add-hook 'haskell-mode-local-vars-hook #'lsp! 'append)
  (add-hook 'haskell-literate-mode-local-vars-hook #'lsp! 'append)
  (after! lsp-mode (require 'lsp-haskell))
  :config
  ;; Does some strange indentation if it pastes in the snippet
  (setq-hook! 'haskell-mode-hook yas-indent-line 'fixed)
  )
