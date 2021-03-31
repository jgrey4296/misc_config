
(after! evil
  (evil-ex-define-cmd "tweet" '+jg-twitter-tweet)
  (evil-ex-define-cmd "image" '+jg-twitter-twitter-add-picture)
  ;; TODO (spacemacs/set-leader-keys "a U t" '+jg-twitter-open_user)

  (load! "+bindings")
  )

(use-package! tramp
  :init
  (defvar tramp-crypt-directories nil)
  (defvar tramp-crypt-enabled nil)
  :config
  (tramp-register-file-name-handlers)
  )

(load! "+vars")
(load! "+funcs")

