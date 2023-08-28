;;; emacs/ibuffer/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(local-load! "+specs")
(local-load! "+spec-defs")
(defer-load! (jg-bindings-total ibuffer) "+bindings")
(defer-load! jg-evil-ex-bindings "+evil-ex")

(use-package! ibuffer
  :config
  (evil-set-initial-state 'ibuffer-mode 'normal)
  (setq-hook! 'ibuffer-hook
    evil-disable-insert-state-bindings t
    )
  )

(use-package! ibuffer-projectile
  ;; Group ibuffer's list by project root
  ;; :hook (ibuffer . ibuffer-projectile-set-filter-groups)
  :config
  (setq ibuffer-projectile-prefix "Project: ")
  )

(use-package! ibuffer-vc
  :after ibuffer
  )
