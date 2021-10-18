;;; util/jg-mail/config.el -*- lexical-binding: t; -*-

(load! "+bindings")
(load! "+funcs")

(set-email-account! "jgrey4296"
                    '((mu4e-sent-folder             . "/[Gmail].Sent Mail")
                      (mu4e-drafts-folder           . "/Drafts")
                      (mu4e-trash-folder            . "/Trash")
                      (mu4e-refile-folder           . "/[Gmail].All Mail")
                      (smtpmail-smtp-user           . "jgrey4296")
                      (user-mail-address            . "jgrey4296@gmail.com")
                      (smtpmail-smtp-service        . 465)
                      (message-send-mail-function   . smtpmail-send-it)
                      (smtpmail-default-smtp-server . "smtp.gmail.com")
                      (smtpmail-smtp-server         . "smtp.gmail.com")
                      (smtpmail-stream-type         . starttls)
                      (smtpmail-smtp-service        . 587)
                      (auth-sources                 . ("~/.authinfo" macos-keychain-generic macos-keychain-internet "~/authinfo.gpg"))
                      (mu4e-compose-signature . "---\nJohn"))
                    t)

(set-email-account! "local"
                    `((user-mail-address . "johngrey@Johns-Mac-mini.local" )
                      (smtpmail-smtp-server . ,(system-name))
                      (message-send-mail-function . sendmail-send-it)
                      )
                    )

(use-package! mu4e
  :hook (mu4e-main-mode . +jg-mail-override-mu4e-hook)
  )

(use-package! rmail
  :commands rmail
  :after evil
  :config
  (evil-make-intercept-map rmail-mode-map)
  (map! :map rmail-mode-map
        "j" nil
        "k" nil
        "v" nil
        "e" nil
        "q" nil
        "q" #'quit-window
        "n" #'rmail-next-undeleted-message
        "p" #'rmail-previous-undeleted-message
        "q" #'quit-window
        "Q" #'rmail-quit
        "d" #'rmail-delete-forward
        )
  )

(use-package! rmailsum
  :commands rmail-summary
  :after evil
  :config
  (evil-make-intercept-map rmail-summary-mode-map)
  (map! :map rmail-summary-mode-map
        [menu-bar] nil
        "j"  nil
        "k"  nil
        "d"  nil
        "q"  #'quit-window
        "Q"  #'rmail-summary-quit
        "d"  #'rmail-summary-delete-forward
        )
  )
