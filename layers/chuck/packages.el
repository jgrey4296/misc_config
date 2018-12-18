;; chuck packages.el
;; loads second

(defconst chuck-packages
  '(
    (chuck-mode :location local)
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



(defun chuck/init-chuck-mode ()
 (use-package chuck-mode
   :commands (chuck-mode)
   )
)
