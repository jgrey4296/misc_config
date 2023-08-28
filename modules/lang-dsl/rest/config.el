;;; lang/rest/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(use-package! restclient
  :commands restclient-mode
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
  :after restclient
  )

(use-package! restclient-jq
  :after restclient
  )
