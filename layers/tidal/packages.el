;; tidal packages.el
;; loads second

(defconst tidal-packages
  '(
    (tidal-mode :location local)
    ;; package from EPA
    ;; eg: some-package
    ;; (some-package :location elpa)
    ;; (some-package :location local)
    ;; (some-package :location (recipe :fetcher github :repo "some/repo"))
    ;;(some-package :excluded t)
    )

  )

;; (def <layer>/pre-init-<package>)
;; (def <layer>/init-<package>)
;; (def <layer>/post-init-<package>)

(def tidal/init-tidal-mode
     (use-package tidal-mode
       :defer t
       :mode "\\.tidal\\$"

       )
     )
