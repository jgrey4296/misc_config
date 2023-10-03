;; -*- no-byte-compile: t; -*-
;;; lang/erlang/packages.el

(package! erlang)
(package! elixir-mode)
(package! alchemist)
(package! exunit)
(package! flycheck-credo)
(package! ob-erlang           :recipe (:host github :repo "jgrey4296/misc-modes" :files ("org-babels/ob-erlang.el") :local-repo "misc-modes"))
