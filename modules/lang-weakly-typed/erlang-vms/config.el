;;; lang/erlang/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(after! jg-bindings-total
  (load! "+bindings")
  )
(load! "+repl")

(use-package! erlang
  :defer t
  :mode ("\\.erlang\\'" . erlang-mode)
  :mode ("/rebar\\.config\\(?:\\.script\\)?\\'" . erlang-mode)
  :mode ("/\\(?:app\\|sys\\)\\.config\\'" . erlang-mode)
  :config
  (setq erlang-root-dir "/usr/local/opt/erlang"
        exec-path (cons "/usr/local/opt/erlang/bin" exec-path)
        )

  (when (modulep! +lsp)
    (add-hook 'erlang-mode-local-vars-hook #'lsp! 'append))

  (when (modulep! +tree-sitter)
    (add-hook 'erlang-mode-local-vars-hook #'tree-sitter! 'append))
  )

(after! projectile
  (add-to-list 'projectile-project-root-files "mix.exs"))

;;
;;; Packages

(use-package! elixir-mode
  :defer t
  :init
  ;; Disable default smartparens config. There are too many pairs; we only want
  ;; a subset of them (defined below).
  (provide 'smartparens-elixir)
  :config
  (set-ligatures! 'elixir-mode
    ;; Functional
    :def "def"
    :lambda "fn"
    ;; :src_block "do"
    ;; :src_block_end "end"
    ;; Flow
    :not "!"
    :in "in" :not-in "not in"
    :and "and" :or "or"
    :for "for"
    :return "return" :yield "use")

  ;; ...and only complete the basics
  (sp-with-modes 'elixir-mode
    (sp-local-pair "do" "end"
                   :when '(("RET" "<evil-ret>"))
                   :unless '(sp-in-comment-p sp-in-string-p)
                   :post-handlers '("||\n[i]"))
    (sp-local-pair "do " " end" :unless '(sp-in-comment-p sp-in-string-p))
    (sp-local-pair "fn " " end" :unless '(sp-in-comment-p sp-in-string-p)))

  (when (modulep! +lsp)
    (add-hook 'elixir-mode-local-vars-hook #'lsp! 'append)
    (after! lsp-mode
      (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]_build\\'")))

  (when (modulep! +tree-sitter)
    (add-hook 'elixir-mode-local-vars-hook #'tree-sitter! 'append))

  (after! highlight-numbers
    (puthash 'elixir-mode
             "\\_<-?[[:digit:]]+\\(?:_[[:digit:]]\\{3\\}\\)*\\_>"
             highlight-numbers-modelist)))

(use-package! flycheck-credo
  :when (modulep! :checkers syntax)
  :after elixir-mode
  :config (flycheck-credo-setup))

(use-package! alchemist
  :hook (elixir-mode . alchemist-mode)
  :config
  (spec-handling-add! lookup-handler nil
                      (elixir-mode
                       :definition #'alchemist-goto-definition-at-point
                       :documentation #'alchemist-help-search-at-point
                       )
                      )
  (set-eval-handler! 'elixir-mode #'alchemist-eval-region)
  (set-repl-handler! 'elixir-mode #'alchemist-iex-project-run)
  )

(use-package! alchemist-company
  :when (modulep! :completion company)
  :commands alchemist-company
  :config
  (spec-handling-add! company nil (alchemist-mode (alchemist-company company-yasnippet)))
  ;; Alchemist doesn't use hook symbols to add these backends, so we have to use
  ;; the entire closure to get rid of it.
  (let ((fn (byte-compile (lambda () (add-to-list (make-local-variable 'company-backends) 'alchemist-company)))))
    (remove-hook 'alchemist-mode-hook fn)
    (remove-hook 'alchemist-iex-mode-hook fn)))

(use-package! exunit
  :hook (elixir-mode . exunit-mode)
  )
