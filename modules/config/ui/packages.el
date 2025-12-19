;; -*- no-byte-compile: t; -*-
;;; emacs/jg-ui/packages.el


(package! anzu)
(package! auto-highlight-symbol)
(package! doom-modeline)
(package! highlight-indent-guides)
(package! highlight-parentheses)
(package! highlight-numbers)
(package! hilit-chg)
(package! hl-todo)
(package! rainbow-delimiters)
(package! rainbow-mode)
(package! visual-fill-column)
(package! whitespace)
(package! paren)
(package! hide-mode-line)
(package! fringe :built-in t)

(package! font-lock :built-in t)
(package! font-lock-ext)
(package! font-lock-studio)
(package! font-lock+       :recipe (:host github :repo "emacsmirror/font-lock-plus"))

(package! transient
  :recipe (:host github :repo "magit/transient" :branch "main")
  :pin "453376f2f1...")

(package! treemacs)
(package! treemacs-evil)
(package! treemacs-persp)
(package! treemacs-projectile)
(package! neotree)


(package! elide-head)
