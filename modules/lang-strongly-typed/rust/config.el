;;; lang/rust/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(local-load! "+lsp")
(local-load! "+popup")
(local-load! "+flycheck")
(when (modulep! +llvm) (local-load! "+llvm"))
(local-load! "+c-lang")

(defer-load! jg-bindings-total "+bindings")

(use-package! rust-mode
  :commands rust-mode
  )

(use-package! rust-ts-mode
  :commands rust-ts-mode
  :config
  (add-hook! 'rust-ts-mode #'treesit-fold-mode)
  )

(use-package! rustic
  :commands rustic-mode
  :preface
  ;; Disable rustic's loading to control it
  (setq rustic-load-optional-libraries nil)
  :init
  (after! org-src
    ;; HACK Certainly, `rustic-babel' does this, but the package (and many other
    ;;   rustic packages) must be loaded in order for them to take effect. To lazy
    ;;   load it all, we must do it early:
    (defalias 'org-babel-execute:rust #'org-babel-execute:rustic)
    (add-to-list 'org-src-lang-modes '("rust" . rustic))
    )
  :config
  (after! flycheck
    (add-to-list 'flycheck-checkers 'rustic-clippy)
    )

  (add-hook! 'rust-mode-hook
             #'flycheck-mode
             #'rainbow-delimiters-mode
             #'maybe-rust-test-minor-mode
             #'hs-minor-mode
             #'tree-sitter!
             #'librarian-insert-minor-mode
             )

  (setq-hook! 'rustic-mode-hook
    flycheck--automatically-enabled-checkers '(rustic-clippy)
    flycheck--automatically-disabled-checkers '()
    code-shy-block-depth 1
    code-shy-fold-patterns '("%s //--// %s" "%s //--// %s %s")
    )
)

(use-package! rustic-compile
  :after rustic
  )

(use-package! rustic-cargo
  :after rustic
  )

(use-package! rustic-doc
  :after rustic
  )

(use-package! rustic-clippy
  :after rustic
  )

(use-package! rustic-comint
  :after rustic
  )

(use-package! rustic-babel
  :after (rustic org)
  )

(use-package! rustic-rustfmt
  :after rustic
  )

(use-package! rustic-rustfix
  :after rustic
  )

(use-package! rustic-playground
  :disabled t
  :after rustic
  )

(use-package! rustic-expand
  :after rustic
  )

(use-package! rustic-spellcheck
  :after rustic
  )
