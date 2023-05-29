;;; lang/nix/config.el -*- lexical-binding: t; -*-

(defer-load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(after! tramp
  (add-to-list 'tramp-remote-path "/run/current-system/sw/bin"))

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
