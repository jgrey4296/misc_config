(setq-default tidal-interpreter "/usr/local/bin/ghci"
              ;; tidal-interpreter-arguments
              ;; (list "-ghci-script" (expand-file-name "~/github/languageLearning/tidal/.ghci"))
              sclang-auto-scroll-post-buffer t
              sclang-help-path (quote ("/Applications/SuperCollider/Help"))
              sclang-program "sclang"
              sclang-rtf-editor-program "emacs"
              sclang-show-workspace-on-startup nil
              sclang-library-configuration-file (expand-file-name "~/.doom.d/modules/jg-music-layer/sclang.yaml")
              sclang-udp-port 57120
              sclang-eval-line-forward nil
              sclang-runtime-directory (expand-file-name "~/.sclang/")
              sclang-boot-file (expand-file-name "~/.doom.d/modules/jg-music-layer/startup.scd")

              jg-music-layer-tidal-workspace "Tidal Workspace"
              jg-music-layer-sclang-workspace "SCLang Workspace"

              )
(use-package! jg-music-layer-minor-mode
  :commands (jg-music-layer-minor-mode jg-music-layer-on global-jg-music-layer-mode)
  :config
  (message "Configuring Jg-Music-Layer Minor")
  (map! :mode jg-music-layer-minor-mode
        ". h" 'jg-music-layer-minor/hush
        ". e" 'jg-music-layer-minor/jg-music-layer-eval-selection
        ". r" 'jg-music-layer-minor/sclang-restart
        ". R" 'jg-music-layer-minor/sclang-recompile
        ". w" 'jg-music-layer/setup-windows
        )
  (add-hook 'jg-music-layer-minor-mode-hook 'jg-music-layer/setup-minor-mode-keys)
  )
(use-package! sclang
  :commands (sclang-mode )
  :config
  (evil-define-key nil sclang-mode-map
    (kbd "C-c [") nil)
  (evil-define-key '(insert normal) sclang-mode-map
    (kbd "C-c [") nil)
  (evil-define-key '(normal insert) sclang-mode-map
    (kbd "C-c [") 'jg_layer/insert-lparen
    (kbd "C-c C-c") 'jg-music-layer-minor/jg-music-layer-eval-line)
  (evil-define-key '(visual) sclang-mode-map
    (kbd "C-c C-c") 'jg-music-layer-minor/jg-music-layer-eval-selection)
  )
(use-package! tidal
  :commands (tidal-mode tidal-start-haskell)
  :config
  (evil-define-key nil tidal-mode-map
    (kbd "C-c C-c") nil)
  (evil-define-key '(normal insert visual) tidal-mode-map
    (kbd "C-c C-c") nil)
  (evil-define-key '(normal insert) tidal-mode-map
    (kbd "C-c C-c") 'jg-music-layer-minor/jg-music-layer-eval-line)
  (evil-define-key '(visual) tidal-mode-map
    (kbd "C-c C-c") 'jg-music-layer-minor/jg-music-layer-eval-selection)
    )
(use-package! chuck-mode
  :commands (chuck-mode)
  )

(after! evil
  (load! "+bindings")
  )
