;;; util/jg-mail/config.el -*- lexical-binding: t; -*-

(after! evil
  (load! "+bindings")
)
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
  )

(use-package! rmailsum
  :defer
  :config
  (setq rmail-summary-font-lock-keywords
    `(("^ *[0-9]+D.*" . font-lock-string-face)			; Deleted.
      ("^ *[0-9]+-.*" . font-lock-type-face)			; Unread.
      ;; Neither of the below will be highlighted if either of the above are:
      ("^ *[0-9]+[^D-] \\(......\\)" 1 font-lock-keyword-face)	; Date.
      ("{ \\([^\n}]+\\) }" 1 font-lock-comment-face)  ; Labels
      (,(rx (+ alnum) ?@ (+ (any "a-z" "A-Z" "-" ?. )))
       0 'homoglyph)
      ("Subject: " 0 'hi-green-b)
      )
    )
  (load! "+advice")
  )
