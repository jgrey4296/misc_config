;; -*- no-byte-compile: t; -*-
;;; ui/hydra/packages.el

(package! hydra)
(package! hydra-macros :recipe `(:host github :repo "jgrey4296/jg-el-macros" :files ("hydra-macros") :local-repo "jg-el-macros"))
