;;; lang/nix/config.el -*- lexical-binding: t; -*-

(after! tramp
  (add-to-list 'tramp-remote-path "/run/current-system/sw/bin"))


;;
;;; Plugins

(use-package! nix-mode
  :defer t
  :interpreter ("\\(?:cached-\\)?nix-shell" . +nix-shell-init-mode)
  :mode "\\.nix\\'"
  :init
  ;; Treat flake.lock files as json. Fall back to js-mode because it's faster
  ;; than js2-mode, and its extra features aren't needed there.
  (add-to-list 'auto-mode-alist
               (cons "/flake\\.lock\\'"
                     (if (modulep! :lang json)
                         'json-mode
                       'js-mode)))
  :config
  (set-repl-handler! 'nix-mode #'+nix/open-repl)
  (spec-handling-add! company nil (nix-mode company-nixos-options))
  (spec-handling-add! lookup-handler nil
                      (nix-mode
                       :documentation '(+nix/lookup-option :async t)
                       )
                      )
  (spec-handling-add! popup nil
                      ('nix
                       ("^\\*nixos-options-doc\\*$" :ttl 0 :quit t)
                       )
                      )

  ;; Fix #3927: disable idle completion because `company-nixos-options' is
  ;; dreadfully slow. It can still be invoked manually..
  (setq-hook! 'nix-mode-hook company-idle-delay nil)

  (when (modulep! +lsp)
    (add-hook 'nix-mode-local-vars-hook #'lsp! 'append))
  (when (modulep! +tree-sitter)
    (add-hook 'nix-mode-local-vars-hook #'tree-sitter! 'append))

  )


(use-package! nix-update
  :commands nix-update-fetch)


(use-package! nix-repl
  :commands nix-repl-show)

(spec-handling-add! lookup-regular nil
                    (nix-mode
                     ("Nix Reference" . "https://nixos.org/learn.html")
                     ("Nix Language" . "https://nixos.org/guides/nix-language.html")
                     )
                    )
