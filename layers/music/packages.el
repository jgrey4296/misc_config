;; sclang packages.el
;; loads second

(defconst music-packages
  '(
    (sclang :location local)
    (tidal :location local)
    )
  )

(defun music/init-sclang()
  (use-package sclang
    :commands (sclang-mode )
    )
  )

(defun music/init-tidal ()
  (use-package tidal
    :commands (tidal-mode tidal-start-haskell)
    )
  )
