;; -*- no-byte-compile: t; -*-
;;; editor/evil/packages.el

(package! evil)
(package! evil-args)
(package! evil-easymotion)
(package! evil-embrace)
(package! evil-escape :recipe (:host github :repo "hlissner/evil-escape"))
(package! evil-exchange)
(package! evil-indent-plus)
(package! evil-lion)
(package! evil-nerd-commenter)
(package! evil-numbers)
(package! evil-snipe)
(package! evil-surround)
(package! evil-textobj-anyblock :recipe (:host github :repo "willghatch/evil-textobj-anyblock" :branch "fix-inner-block"))
(package! evil-traces)
(package! evil-visualstar)
(package! exato)
(package! evil-quick-diff :recipe (:host github :repo "rgrinberg/evil-quick-diff"))
(package! evil-quickscope)
(package! evil-iedit-state)

(when (modulep! +everywhere)
  ;; `evil-collection-neotree' uses the `neotree-make-executor' macro, but this
  ;; requires neotree be available during byte-compilation (while installing).
  (when (modulep! :ui neotree)
    (package! neotree)
    (autoload 'neotree-make-executor "neotree" nil nil 'macro))

  (package! evil-collection)
  )
