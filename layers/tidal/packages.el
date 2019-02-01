;; tidal packages.el
;; loads second

(defconst tidal-packages
  '(
    (tidal :location local)
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

(defun tidal/init-tidal ()
     (use-package tidal
       :commands (tidal-mode tidal-start-haskell)
       ;; Pre-loading setup:
       ;; :init ()
       ;; Post-loading setup:
       ;; :config ()

       )
     )

(defun tidal/post-init-tidal ()

  )
