;;; util/jg-mail/config.el -*- lexical-binding: t; -*-

(after! jg-bindings-total
  (load! "+bindings")
)
(load! "+funcs")


(use-package! mu4e
  :hook (mu4e-main-mode . +jg-mail-override-mu4e-hook)
  )

(use-package! rmail
  :commands rmail
  :after evil
  )

(use-package! rmailsum
  :defer t
  :config
  (load! "+advice")
  )
