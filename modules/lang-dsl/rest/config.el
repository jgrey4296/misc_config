;;; lang/rest/config.el -*- lexical-binding: t; -*-

(after! jg-bindings-total
  (load! "+bindings")
  )

(use-package! restclient
  :defer t
  :mode ("\\.http\\'" . restclient-mode)
  ;; line numbers aren't enabled by default in fundamental-mode-derived modes
  :hook (restclient-mode . display-line-numbers-mode)
  :config
  (spec-handling-add! popup nil
                      ('rest
                       ("^\\*HTTP Response" :size 0.4 :quit 'other)
                       )
                      )

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
  :when (modulep! :completion company)
  :after restclient
  :config
  (spec-handling-add! company nil
                      '(restclient-mode (:mode . #'company-restclient))
                      )
  )

(use-package! restclient-jq
  :defer t
  :when (modulep! +jq)
  :after restclient)

(use-package! jq-mode
  :defer t
  :when (modulep! +jq)
  :after restclient-jq)
