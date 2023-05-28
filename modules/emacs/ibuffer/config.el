;;; emacs/ibuffer/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+specs")
(load! "+spec-defs")
(after! (jg-bindings-total ibuffer)
  (load! "+bindings")
 )

(use-package! ibuffer
  :config
  (load! "+format")
  (load! "+sorting")
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
