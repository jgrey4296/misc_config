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
(after! rainbow-mode
  ;; (spacemacs/set-leader-keys "t C r" 'rainbow-mode)
  (add-hook 'prog-mode-hook 'rainbow-mode)
)
(after! evil-string-inflection
  (define-key evil-normal-state-map
    "g '" 'evil-operator-string-inflection
    )
  )
(after! free-keys
  (map! :leader
        :prefix ("a U" . "Utilities")
        "f k" 'free-keys
        "f p" 'free-keys-set-prefix
        )
  )
(after! dired-quick-sort
    (dired-quick-sort-setup)
    )
(after! highlight-parentheses
  (setq hl-paren-colors '("color-16" "color-16" "color-16" "color-16")
        hl-paren-background-colors '("Springgreen3" "color-26" "color-91" "IndianRed3"))
  )
(after! undo-tree
  (map! :leader
        "b u" 'undo-tree-visualize
    )
  )
