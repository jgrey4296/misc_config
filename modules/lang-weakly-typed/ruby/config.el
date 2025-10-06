;;; lang/ruby/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(local-load! "+support")
(local-load! "+tooling")
(local-load! "+testing")

(defer-load! jg-bindings-total "+bindings")
(after! projectile
  (add-to-list 'projectile-project-root-files "Gemfile"))

(use-package! ruby-mode  ; built-in
  :commands ruby-mode
  ;; Other extensions are already registered in `auto-mode-alist' by `ruby-mode'
  :interpreter "j?ruby\\(?:[0-9.]+\\)"
  :config
  (setq ruby-insert-encoding-magic-comment nil)

  (add-hook 'ruby-mode-local-vars-hook #'tree-sitter!)

  (after! inf-ruby
    (add-hook 'inf-ruby-mode-hook #'doom-mark-buffer-as-real-h)
    ;; switch to inf-ruby from compile if we detect a breakpoint has been hit
    (add-hook 'compilation-filter-hook #'inf-ruby-auto-enter))

  ;; so class and module pairs work
  (setq-hook! 'ruby-mode-hook sp-max-pair-length 6)
  )

;; TODO ruby repl
