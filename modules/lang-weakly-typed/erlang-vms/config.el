;;; lang/erlang/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(local-load! "+extra-config")
(defer-load! jg-bindings-total "+bindings")
(after! projectile
  (add-to-list 'projectile-project-root-files "mix.exs"))

(use-package! erlang
  :commands erlang-mode
  :config
  (setq erlang-root-dir "/usr/local/opt/erlang"
        exec-path (cons "/usr/local/opt/erlang/bin" exec-path)
        )
  (add-hook 'erlang-mode-hook #'tree-sitter!)
  )

(use-package! elixir-mode
  :commands elixir-mode
  :init
  ;; Disable default smartparens config. There are too many pairs; we only want
  ;; a subset of them (defined below).
  (provide 'smartparens-elixir)
  :config
  ;; ...and only complete the basics
  (sp-with-modes 'elixir-mode
    (sp-local-pair "do" "end"
                   :when '(("RET" "<evil-ret>"))
                   :unless '(sp-in-comment-p sp-in-string-p)
                   :post-handlers '("||\n[i]"))
    (sp-local-pair "do " " end" :unless '(sp-in-comment-p sp-in-string-p))
    (sp-local-pair "fn " " end" :unless '(sp-in-comment-p sp-in-string-p)))

  ;; (when (modulep! +lsp)
  ;;   (add-hook 'elixir-mode-local-vars-hook #'lsp! 'append)
  ;;   (after! lsp-mode
  ;;     (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]_build\\'")))

  (add-hook 'elixir-mode-hook #'tree-sitter!)
  (add-hook 'elixir-mode-hook #'librarian-insert-minor-mode)
  (add-hook 'elixir-mode-hook #'treesit-fold-mode)

  (add-hook 'elixir-ts-mode-hook #'tree-sitter!)
  (add-hook 'elixir-ts-mode-hook #'librarian-insert-minor-mode)
  (add-hook 'elixir-ts-mode-hook #'treesit-fold-mode)

  (after! highlight-numbers
    (puthash 'elixir-mode
             "\\_<-?[[:digit:]]+\\(?:_[[:digit:]]\\{3\\}\\)*\\_>"
             highlight-numbers-modelist))
  )

(use-package! exunit
  :after elixir-mode
  :hook (elixir-mode . exunit-mode)
  )

(use-package! heex-ts-mode)
