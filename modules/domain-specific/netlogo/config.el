;; netlogo config.el
;; loaded fourth

;;(configuration-layer/declare-layer)
;;(configuration-layer/declare-layers)
;;(configuration-layer/layer-usedp)
;;(configuration-layer/package-usedp)
;; (defun <layer>/pre-init-<package>)
;; (defun <layer>/init-<package>)
;; (defun <layer>/post-init-<package>)

(defun netlogo/init-netlogo ()
  (use-package netlogo-mode
    :commands (netlogo-mode)
    ))
