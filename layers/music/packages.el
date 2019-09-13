;; sclang packages.el
;; loads second

(defconst music-packages
  '(
    (sclang :location local)
    (tidal :location local)
    (chuck-mode :location local)
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
