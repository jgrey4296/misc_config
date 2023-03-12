(load! "+vars")
(after! jg-bindings-total
  (load! "+bindings")
)
(use-package! music-minor-mode
  :commands (music-minor-mode music-minor/music-on global-music-mode)
  :config
  (message "Configuring Jg-Music Minor")
  (map! :map music-minor-mode-map
        ". h" 'music-minor/hush
        ". e" 'music-minor/music-eval-selection
        ". r" 'music-minor/sclang-restart
        ". R" 'music-minor/sclang-recompile
        ". w" '+jg-music/setup-windows
        )
  (add-hook 'music-minor-mode-hook '+jg-music/setup-minor-mode-keys)
  )
(use-package! sclang
  :commands (sclang-mode )
  :config
  (evil-define-key nil sclang-mode-map
    (kbd "C-c [") nil)
  (evil-define-key '(insert normal) sclang-mode-map
    (kbd "C-c [") nil)
  (evil-define-key '(normal insert) sclang-mode-map
    (kbd "C-c [") '+jg-text-insert-lparen
    (kbd "C-c C-c") 'music-minor/music-eval-line)
  (evil-define-key '(visual) sclang-mode-map
    (kbd "C-c C-c") 'music-minor/music-eval-selection)
  )
(use-package! tidal
  :commands (tidal-mode tidal-start-haskell)
  :config
  (evil-define-key nil tidal-mode-map
    (kbd "C-c C-c") nil)
  (evil-define-key '(normal insert visual) tidal-mode-map
    (kbd "C-c C-c") nil)
  (evil-define-key '(normal insert) tidal-mode-map
    (kbd "C-c C-c") 'music-minor/music-eval-line)
  (evil-define-key '(visual) tidal-mode-map
    (kbd "C-c C-c") 'music-minor/music-eval-selection)
    )
(use-package! chuck-mode
  :commands (chuck-mode)
  )

(use-package! csound-mode)
