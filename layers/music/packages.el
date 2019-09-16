;; sclang packages.el
;; loads second

(defconst music-packages
  '(
    (sclang :location local)
    (tidal :location local)
    (chuck-mode :location local)
    (music-minor-mode :location local)
    )
  )

(defun music/init-music-minor-mode ()
  (use-package music-minor-mode
    :commands (music-minor-mode music-on global-music-mode)
    :config
    (message "Configuring Music Minor")
    (spacemacs/set-leader-keys-for-minor-mode 'music-minor-mode
      ". h" 'music-minor/hush
      ". e" 'music-minor/music-eval-selection
      ". r" 'music-minor/sclang-restart
      ". R" 'music-minor/sclang-recompile
      ". w" 'music/setup-windows
      )
    (add-hook 'music-minor-mode-hook (lambda ()
                                       (spacemacs/set-leader-keys
                                         "a . h" 'music-minor/hush
                                         "a . q" 'music-minor/quit
                                         "a . w" 'music/setup-windows
                                         )
                                       )
              )
    )
  )

(defun music/init-sclang()
  (use-package sclang
    :commands (sclang-mode )
    :config
    (spacemacs/set-leader-keys-for-major-mode

      )
    )
  )

(defun music/init-tidal ()
  (use-package tidal
    :commands (tidal-mode tidal-start-haskell)
    :config
    (spacemacs/set-leader-keys-for-major-mode

      )
    )
  )

(defun music/init-chuck-mode ()
  (use-package chuck-mode
    :commands (chuck-mode)
    )
  )
