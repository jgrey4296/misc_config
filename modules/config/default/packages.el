;; -*- no-byte-compile: t; -*-
;;; config/default/packages.el

(package! link-hint)
(package! expand-region)
(package! spec-handling :recipe (:host github :repo "jgrey4296/spec-handling"))
(package! vimrc-mode)
(package! fsm)
(package! xclip :pin "a1ac607f75a250dddf49866918bb493884451130")
(package! evil-terminal-cursor-changer :pin "12ea9c0438c67e560b3866dc78b5c7d1d93f8cc5")
(package! epa :built-in t)
(package! epa-hook :built-in t)

(package! s)
(package! a)
(package! f)
(package! compat :pin nil :recipe (:host github :repo "emacs-compat/compat"))

(package! macro-tools :recipe (:host github :repo "jgrey4296/macro-tools"))
