;;; util/jg-mail/config.el -*- lexical-binding: t; -*-

(defer-load! jg-bindings-total "+bindings")


(use-package! mu4e
  :commands mu4e
  :hook (mu4e-main-mode . +jg-mail-override-mu4e-hook)
  )

(use-package! rmail
  :commands rmail
  :after evil
  )

(use-package! rmailsum
  :after rmail
  )
