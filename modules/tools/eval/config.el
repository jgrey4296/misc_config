;;; tools/eval/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+spec-defs")
(defer-load! jg-bindings-total "+bindings")

(use-package! quickrun
  :config
  (setq quickrun-focus-p nil)

  (add-hook! 'quickrun-after-run-hook
             #'+eval-quickrun-shrink-window-h
             #' +eval-quickrun-scroll-to-bof-h
                )
  )

(use-package! eros
  :when (modulep! +overlay)
  :hook (emacs-lisp-mode . eros-mode))
