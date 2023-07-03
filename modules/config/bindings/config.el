;;; config/default/config.el -*- lexical-binding: t; -*-

(load! "+vars")

(after! (evil faster-whichkey)
  (load! "+leader-bindings")
  (load! "submaps/+evil-bindings")
  (load! "+misc-bindings")
  (provide 'jg-bindings-total)
  (message "Core JG Bindings Set")
  )

(use-package! faster-whichkey
  :after (which-key general)
  )
