;;; +minor-mode.el -*- lexical-binding: t; no-byte-compile: t;-*-
;;-- header
;; File Commentary:
;;
;;
;;-- end header
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
(setq-default
 jg-music-tidal-workspace "Tidal Workspace"
 jg-music-sclang-workspace "SCLang Workspace"
 )

;;; +minor-mode.el ends here
