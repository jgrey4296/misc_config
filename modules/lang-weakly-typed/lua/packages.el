;; -*- no-byte-compile: t; -*-
;;; lang/lua/packages.el

(package! lua-mode)
(package! moonscript)
(package! flycheck-moonscript :recipe (:host github :repo "hlissner/emacs-flycheck-moonscript"))
(package! fennel-mode)
(package! company-lua)
