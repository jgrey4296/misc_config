
(load! "+bindings")
(load! "+vars")
(load! "+funcs")
(load! "+downloader")

(use-package! tramp
  :init
  (defvar tramp-crypt-directories nil)
  (defvar tramp-crypt-enabled nil)
  :config
  (tramp-register-file-name-handlers)
  )

(add-hook! doom-first-input
           #'+twitter-binding-hook
           #'+twitter-evil-ex-binding-hook
           )
