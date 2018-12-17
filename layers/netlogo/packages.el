;; netlogo packages.el
;; loads second

(defconst netlogo-packages
  '(
    (netlogo-mode :location local)
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

(def netlogo/init-netlogo
     (use-package netlogo-mode
       :defer t
       :mode "\\.nlogo\\'"
       :commands (netlogo-mode)
       :init ()
       :config ()
     )
