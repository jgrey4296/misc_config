;;; tools/eval/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(local-load! "+spec-defs")
(defer-load! jg-bindings-total "+bindings")
(defer-load! jg-evil-ex-bindings "+evil-ex")

(use-package! quickrun
  :config
  (setq quickrun-focus-p nil)

  (add-hook! 'quickrun-after-run-hook
             #'+eval-quickrun-shrink-window-h
             #' +eval-quickrun-scroll-to-bof-h
                )
  )

(use-package! eros
  :commands eros-mode
  :hook (emacs-lisp-mode . eros-mode)
  )
