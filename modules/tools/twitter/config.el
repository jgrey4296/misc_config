;; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;
(after! jg-bindings-total
  (load! "+bindings")
  (load! "+motions")
  )
(load! "+vars")
(load! "+funcs")
(load! "+downloader")
(load! "+tweet-minor-mode")

(use-package! tramp
  :defer t
  :init
  (defvar tramp-crypt-directories nil)
  (defvar tramp-crypt-enabled nil)
  :config
  (tramp-register-file-name-handlers)
  )