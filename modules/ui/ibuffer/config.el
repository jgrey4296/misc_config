;;; emacs/ibuffer/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(local-load! "+spec-defs")
(local-load! "+specs")
(defer-load! (jg-bindings-total ibuffer) "+bindings")
(defer-load! jg-evil-ex-bindings "+evil-ex")

(advice-add 'ibuffer-find-file         :override #'+ibuffer--use-counsel-maybe-a)
(advice-add 'ibuffer-do-sort-by-marked :before #'+ibuffer-populate-marked-list-for-sorting)

(use-package! ibuffer
  :config
  (local-load! "+filters")
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
