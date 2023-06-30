;;; lang/rest/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(use-package! restclient
  :defer t
  ;; line numbers aren't enabled by default in fundamental-mode-derived modes
  :hook (restclient-mode . display-line-numbers-mode)
  :config

  (setq-hook! 'restclient-mode-hook
    imenu-generic-expression '((nil "^[A-Z]+\s+.+" 0)))

  (defadvice! +rest--permit-self-signed-ssl-a (fn &rest args)
    "Forces underlying SSL verification to prompt for self-signed or invalid
certs, rather than reject them silently."
    :around #'restclient-http-do
    (require 'gnutls)
    (let (gnutls-verify-error)
      (apply fn args)))

 )

(use-package! company-restclient
  :defer t
  :after restclient
  :config
  )

(use-package! restclient-jq
  :defer t
  :after restclient)

(use-package! jq-mode
  :defer t
  :after restclient-jq)
