;;; util/jg-mail/config.el -*- lexical-binding: t; -*-


(after! evil
  (load! "+bindings")
  )

(set-email-account! "jgrey4296"
                    '((mu4e-sent-folder       . "/[Gmail].Sent Mail")
                      (mu4e-drafts-folder     . "/Drafts")
                      (mu4e-trash-folder      . "/Trash")
                      (mu4e-refile-folder     . "/[Gmail].All Mail")
                      (smtpmail-smtp-user     . "jgrey4296")
                      (user-mail-address      . "jgrey4296@gmail.com")    ;; only needed for mu < 1.4
                      (mu4e-compose-signature . "---\nJohn"))
                    t)

(after! mu4e
  (setq-default smtpmail-smtp-service 465
                message-send-mail-function   'smtpmail-send-it
                smtpmail-default-smtp-server "smtp.gmail.com"
                smtpmail-smtp-server         "smtp.gmail.com"
                smtpmail-stream-type  'starttls
                smtpmail-smtp-service 587
                auth-sources '("~/.authinfo"
                               macos-keychain-generic
                               macos-keychain-internet
                               "~/authinfo.gpg")
                )

  (setq mu4e-use-fancy-chars t
        mu4e-headers-draft-mark     '("D" . "D")
        mu4e-headers-flagged-mark   '("F" . "F")
        mu4e-headers-new-mark       '("N" . "N")
        mu4e-headers-passed-mark    '("P" . "P")
        mu4e-headers-replied-mark   '("R" . "R")
        mu4e-headers-seen-mark      '("S" . "S")
        mu4e-headers-trashed-mark   '("T" . "T")
        mu4e-headers-attach-mark    '("a" . "a")
        mu4e-headers-encrypted-mark '("x" . "x")
        mu4e-headers-signed-mark    '("s" . "s")
        mu4e-headers-unread-mark    '("u" . "u"))

  (setq auth-source-backend-parser-functions
        '(auth-source-backends-parser-secrets
          auth-source-backends-parser-file))
)
