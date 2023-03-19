;; -*- no-byte-compile: t; -*-
;;; lang/latex/packages.el

(package! tex-mode)
(package! auctex :recipe (:files ("*.el" "*.info" "dir" "doc" "etc" "images" "latex" "style")))
(package! adaptive-wrap)
(package! latex-preview-pane)
(when (modulep! :editor evil +everywhere)
  (package! evil-tex))

(when (modulep! +latexmk)
  (package! auctex-latexmk))

(when (modulep! +cdlatex)
  (package! cdlatex))

(when (modulep! :completion company)
  (package! company-auctex)
  (package! company-reftex)
  (package! company-math)
  )
