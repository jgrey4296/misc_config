;; tidal config.el
;; loaded fourth

;;(configuration-layer/declare-layer)
;;(configuration-layer/declare-layers)
;;(configuration-layer/layer-usedp)
;;(configuration-layer/package-usedp)

;;tidal interpreter
(setq-default tidal-interpreter "/usr/local/bin/ghci"
              tidal-interpreter-arguments
              (list "-ghci-script" (expand-file-name "~/github/languageLearning/tidal/.ghci"))
              )
