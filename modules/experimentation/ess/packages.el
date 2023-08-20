;; -*- no-byte-compile: t; -*-
;;; lang/ess/packages.el

(package! ess)
(package! ess-R-data-view)
(package! polymode)
(package! poly-R)

(when (modulep! +stan)
  (package! stan-mode)
  (package! eldoc-stan)
  (package! company-stan)
  (package! flycheck-stan)
  )
