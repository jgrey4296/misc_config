;;; +vars.el -*- lexical-binding: t; -*-

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

(pcase system-type
  ('darwin (set-email-account! "local"
                       `((user-mail-address . "johngrey@Johns-Mac-mini.local" )
                                 (smtpmail-smtp-server . ,(system-name))
                                 (message-send-mail-function . sendmail-send-it)
                                 )
                               )
           )
  ('gnu/linux  (set-email-account! "local"
                                   `((user-mail-address . "johngrey@john-UM700" )
                                     (smtpmail-smtp-server . ,(system-name))
                                     (message-send-mail-function . sendmail-send-it)
                                     )
                                   )
               )
  )

(after! rmailsum
  (setq rmail-summary-font-lock-keywords
        `(;; Deleted
          ("^ *[0-9]+D.*" . font-lock-string-face)
          ;; Unread
          ("^ *[0-9]+-.*" . font-lock-type-face)

          ;; None of the below will be highlighted if either of the above are:
          ;; Date
          ("^ *[0-9]+[^D-] \\(......\\)" 1 font-lock-keyword-face)
          ;; Labels
          ("{ \\([^\n}]+\\) }" 1 font-lock-comment-face)
          (,(rx (+ alnum) ?@ (+ (any "a-z" "A-Z" "-" ?. )))
           0 'homoglyph)
          ("Subject: " 0 'hi-green-b)
          )
        )
  )

(setq mail-user-agent 'sendmail-user-agent)

(setq mu4e-maildir (or (getenv "MAILDIR") "~/.cache/mail/")
      mu4e-user-mail-address-list nil
      evil-collection-mu4e-end-region-misc "quit"
      mail-user-agent                          'mu4e-user-agent
      message-mail-user-agent                  'mu4e-user-agent
      mu4e-update-interval        nil
      mu4e-sent-messages-behavior              'sent
      mu4e-hide-index-messages    t
      message-send-mail-function #'smtpmail-send-it       ;; configuration for sending mail
      smtpmail-stream-type                     'starttls
      message-kill-buffer-on-exit t                       ;; close after sending
      mu4e-context-policy                      'pick-first             ;; start with the first (default) context;
      mu4e-compose-context-policy              'ask-if-none            ;; compose with the current context, or ask
      mu4e-completing-read-function #'ivy-completing-read
      mu4e-attachment-dir (concat (expand-file-name "Downloads" "~") "/") ; a trailing / makes it easier to change directory in `read-file-name'
      mu4e-confirm-quit nil
      mu4e-headers-thread-single-orphan-prefix '("─>" . "─▶")
      mu4e-headers-thread-orphan-prefix        '("┬>" . "┬▶ ")
      mu4e-headers-thread-connection-prefix    '("│ " . "│ ")
      mu4e-headers-thread-first-child-prefix   '("├>" . "├▶")
      mu4e-headers-thread-child-prefix         '("├>" . "├▶")
      mu4e-headers-thread-last-child-prefix    '("└>" . "╰▶")
      mu4e-headers-fields '((:account-stripe . 1)
                            (:human-date     . 12)
                            (:flags          . 6) ;; 3 icon flags
                            (:from-or-to     . 25)
                            (:subject))
      )
