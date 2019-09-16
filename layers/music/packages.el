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
    (add-hook 'music-minor-mode-hook 'music/setup-minor-mode-keys)
    )
  )

(defun music/init-sclang()
  (use-package sclang
    :commands (sclang-mode )
    :config
    (evil-define-key nil sclang-mode-map
      (kbd "C-c [") nil)
    (evil-define-key '(insert normal) sclang-mode-map
      (kbd "C-c [") nil)
    (evil-define-key '(normal insert) sclang-mode-map
      (kbd "C-c [") 'jg_layer/insert-lparen
      (kbd "C-c C-c") 'music-minor/music-eval-line)
    (evil-define-key '(visual) sclang-mode-map
      (kbd "C-c C-c") 'music-minor/music-eval-selection)
    )
  )

(defun music/init-tidal ()
  (use-package tidal
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
  )

(defun music/init-chuck-mode ()
  (use-package chuck-mode
    :commands (chuck-mode)
    )
  )
