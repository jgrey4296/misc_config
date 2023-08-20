;;; lang/nix/config.el -*- lexical-binding: t; -*-

(defer-load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(after! tramp
  (add-to-list 'tramp-remote-path "/run/current-system/sw/bin"))

(use-package! nix-mode
  :commands nix-mode
  :interpreter ("\\(?:cached-\\)?nix-shell" . +nix-shell-init-mode)
  :init
  ;; Treat flake.lock files as json. Fall back to js-mode because it's faster
  ;; than js2-mode, and its extra features aren't needed there.
  (add-to-list 'auto-mode-alist
               (cons "/flake\\.lock\\'"
                     (if (modulep! :lang json) 'json-mode 'js-mode)))
  :config

  ;; Fix #3927: disable idle completion because `company-nixos-options' is
  ;; dreadfully slow. It can still be invoked manually..
  (setq-hook! 'nix-mode-hook company-idle-delay nil)

  (add-hook 'nix-mode-hook #'tree-sitter!)

  )

(use-package! nix-update
  :commands nix-update-fetch)

(use-package! nix-repl
  :commands nix-repl-show)
