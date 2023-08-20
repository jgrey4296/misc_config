;; -*- no-byte-compile: t; -*-
;;; lang/web/packages.el

;; +html.el
(package! emmet-mode)
(package! haml-mode)
(package! pug-mode)
(package! slim-mode)
(when (package! web-mode) (package! company-web))

;; +css.el
(package! css-mode)
(package! less-css-mode)
(package! counsel-css)

(package! sass-mode)
(package! stylus-mode)
(package! sws-mode)
(package! rainbow-mode)
(package! counsel-css)
