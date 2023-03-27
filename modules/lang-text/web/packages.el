;; -*- no-byte-compile: t; -*-
;;; lang/web/packages.el

;; +html.el
(package! emmet-mode)
(package! haml-mode)
(package! pug-mode)
(package! slim-mode)
(when (and (package! web-mode) (modulep! :completion company)) (package! company-web))

;; +css.el
(package! css-mode      :built-in t)
(package! less-css-mode :built-in t)

(package! sass-mode)
(package! stylus-mode)
(package! sws-mode)
(package! rainbow-mode)
(when (modulep! :completion ivy) (package! counsel-css))
