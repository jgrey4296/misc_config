;;; util/jg-mail/config.el -*- lexical-binding: t; -*-


(load! "+bindings")
(load! "+funcs")

(set-email-account! "jgrey4296"
                    '((mu4e-sent-folder       . "/[Gmail].Sent Mail")
                      (mu4e-drafts-folder     . "/Drafts")
                      (mu4e-trash-folder      . "/Trash")
                      (mu4e-refile-folder     . "/[Gmail].All Mail")
                      (smtpmail-smtp-user     . "jgrey4296")
                      (user-mail-address      . "jgrey4296@gmail.com")    ;; only needed for mu < 1.4
                      (mu4e-compose-signature . "---\nJohn"))
                    t)

(use-package! mu4e
  :hook (mu4e-main-mode . +jg-mail-override-mu4e-hook)
  )

(add-hook! doom-first-input #'+jg-mail-binding-hook #'+jg-mail-rmail-binding-hook)

;; Rmail binding promotion
(add-transient-hook! #'rmail
  (+jg-mail-rmail-binding-hook)
  (evil-make-intercept-map rmail-mode-map)
  ;; (evil-make-intercept-map rmail-mode-map 'motion)
  ;; (push '(rmail-mode-map) evil-intercept-maps)
)

(add-transient-hook! #'rmail-summary-mode
  (evil-make-intercept-map rmail-summary-mode-map)
  (+jg-mail-rmail-summary-binding-hook)
  )
