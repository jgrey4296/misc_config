 (setq-default shell-default-shell 'shell
               shell-protect-eshell-prompt 0
               shell-enable-smart-eshell t
               )

(after! erlang
  ;; (also has a load path set in root el file)
  (setq erlang-root-dir "/usr/local/opt/erlang"
        exec-path (cons "/usr/local/opt/erlang/bin" exec-path)
        )
  )
(use-package! rainbow-mode
  :defer
  :init
  (map! :leader
        :prefix ("t C" . "Colours")
        "r" 'rainbow-mode)
  (add-hook 'prog-mode-hook 'rainbow-mode)
)
(use-package! evil-string-inflection
  :defer
  :commands evil-operator-string-inflection
  :init
  (map! :n "g '" 'evil-operator-string-inflection)
  )
(use-package! free-keys
  :defer
  :commands (free-keys free-keys-set-prefix)
  :init
  (map! :leader
        (:prefix ("a U" . "Utilities")
         (:prefix ("f" . "free-keys")
          "k" 'free-keys
          "p" 'free-keys-set-prefix
          )
         )
        )
  )
(use-package! dired-quick-sort
  :init
  (dired-quick-sort-setup)
  )
(use-package! highlight-parentheses
  :init
  (setq hl-paren-colors '("color-16" "color-16" "color-16" "color-16")
        hl-paren-background-colors '("Springgreen3" "color-26" "color-91" "IndianRed3"))
  )
(after! undo-tree
  (global-undo-tree-mode 1)
  (map! :leader
        "b u" 'undo-tree-visualize
    )
  )
