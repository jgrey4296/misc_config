;;; lang/erlang/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
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

  (after! highlight-numbers
    (puthash 'elixir-mode
             "\\_<-?[[:digit:]]+\\(?:_[[:digit:]]\\{3\\}\\)*\\_>"
             highlight-numbers-modelist))
  )

(use-package! flycheck-credo
  :after elixir-mode
  :config
  (flycheck-credo-setup)
  )

(use-package! alchemist
  :after elixir-mode
  :hook (elixir-mode . alchemist-mode)
  )

(use-package! alchemist-company
  :after elixir-mode
  :commands alchemist-company
  :config
  ;; Alchemist doesn't use hook symbols to add these backends, so we have to use
  ;; the entire closure to get rid of it.
  (let ((fn (byte-compile (lambda () (add-to-list (make-local-variable 'company-backends) 'alchemist-company)))))
    (remove-hook 'alchemist-mode-hook fn)
    (remove-hook 'alchemist-iex-mode-hook fn)))

(use-package! exunit
  :after elixir-mode
  :hook (elixir-mode . exunit-mode)
  )
