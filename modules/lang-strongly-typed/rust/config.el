;;; lang/rust/config.el -*- lexical-binding: t; -*-

(defer-load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(use-package! rust-mode
  :commands rust-mode
  )

(use-package! rustic
  :commands rustic-mode
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
  (after! flycheck
    (add-to-list 'flycheck-checkers 'rustic-clippy)
    )

  (add-hook! 'rust-mode-hook
             #'rainbow-delimiters-mode
             #'maybe-rust-test-minor-mode
             #'hs-minor-mode
             #'tree-sitter!
             #'general-insert-minor-mode
             )

)

(use-package! llvm-mode
  :defer t
  :load-path "/usr/local/opt/llvm/share/emacs/site-lisp/lvvm/llvm-mode.el"
  ;; clang-format.el clang-include-fixer.el clang-rename.el emacs.el llvm-mode.el tablegen-mode.el
  )
