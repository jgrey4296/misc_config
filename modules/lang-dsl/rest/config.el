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
  (advice-add 'restclient-http-do :around #'+rest--permit-self-signed-ssl-a)
 )

(use-package! company-restclient
  :after restclient
  )

(use-package! restclient-jq
  :after restclient
  )
