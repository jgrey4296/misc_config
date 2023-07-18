;; -*- no-byte-compile: t; -*-
;;; lang/latex/packages.el

(package! auctex :recipe (:files ("*.el" "*.info" "dir" "doc" "etc" "images" "latex" "style")))
(package! adaptive-wrap)
(package! latex-preview-pane)
(package! evil-tex)

(package! cdlatex)

(package! company-auctex)
(package! company-reftex)
(package! company-math)
