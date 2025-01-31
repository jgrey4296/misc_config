;;; lang/rust/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(local-load! "+lsp")

(defer-load! jg-bindings-total "+bindings")

(advice-add 'rustic-install-lsp-client-p :override #'+rust--dont-install-packages-a)

(use-package! rust-mode
  :commands rust-mode
  )

(use-package! rustic
  :commands rustic-mode
  :preface
  (setq rustic-load-optional-libraries nil)
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
    )
)

(use-package! rustic-compile
  :after rustic
  )

(use-package! rustic-popup
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

(use-package! llvm-mode
  :defer t
  :load-path "/usr/local/opt/llvm/share/emacs/site-lisp/lvvm/llvm-mode.el"
  ;; clang-format.el clang-include-fixer.el clang-rename.el emacs.el llvm-mode.el tablegen-mode.el
  )
