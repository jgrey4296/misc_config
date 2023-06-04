;;; lang/rust/config.el -*- lexical-binding: t; -*-

(defer-load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(use-package! rustic
  :defer t
  :mode ("\\.rs$" . rustic-mode)
  :preface
  (after! rustic-lsp
    (remove-hook 'rustic-mode-hook 'rustic-setup-lsp))
  (after! rustic-flycheck
    (remove-hook 'rustic-mode-hook #'flycheck-mode)
    (remove-hook 'rustic-mode-hook #'flymake-mode-off)
    (remove-hook 'flycheck-mode-hook #'rustic-flycheck-setup))
  :init
  (after! org-src
    ;; HACK Certainly, `rustic-babel' does this, but the package (and many other
    ;;   rustic packages) must be loaded in order for them to take effect. To lazy
    ;;   load it all, we must do it early:
    (defalias 'org-babel-execute:rust #'org-babel-execute:rustic)
    (add-to-list 'org-src-lang-modes '("rust" . rustic)))
  :config

  (add-to-list 'flycheck-checkers 'rustic-clippy)

  (add-hook 'rustic-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'rustic-mode-local-vars-hook #'rustic-setup-lsp 'append)
  (add-hook 'rustic-mode-local-vars-hook #'tree-sitter! 'append)
)
