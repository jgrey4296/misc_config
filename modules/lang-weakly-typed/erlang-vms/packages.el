;; -*- no-byte-compile: t; -*-
;;; lang/erlang/packages.el

(package! erlang)
(package! elixir-mode)
(package! alchemist)
(package! exunit)
(package! flycheck-credo)
(package! ob-erlang           :recipe `(:local-repo ,(expand-file-name "packages/org-babel-ext" doom-user-dir) :files ( "ob-erlang.el" )))
