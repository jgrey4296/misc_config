;;; config.el -*- lexical-binding: t; -*-

(when (modulep! +sclang) (local-load! "+sclang"))
(when (modulep! +chuck)  (local-load! "+chuck"))
(when (modulep! +csound) (local-load! "+csound"))
(when (modulep! +tidal)  (local-load! "+tidal"))
(local-load! "+extra")

(defer-load! jg-bindings-total "+bindings")

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


;; --------------------------------------------------

(setq-default
 jg-music-tidal-workspace "Tidal Workspace"
 jg-music-sclang-workspace "SCLang Workspace"
 )
