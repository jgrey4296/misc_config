;;; +vars.el -*- lexical-binding: t; -*-

(spec-handling-setq! mail 50
                     rmail-summary-font-lock-keywords `(;; Deleted
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
                     rmail-file-name (expand-file-name "mail/rmail/rmail" user-cache-dir)
                     rmail-default-file (expand-file-name "mail/rmail/rmail" user-cache-dir)
                     rmail-default-body-file (expand-file-name "mail/rmail/out" user-cache-dir)
                     mail-default-directory (expand-file-name "~")
                     mail-source-directory (expand-file-name "mail" user-cache-dir)
                     smtpmail-queue-dir (expand-file-name "smtp/queue" user-cache-dir)
                     )

(setq evil-collection-mu4e-end-region-misc "quit"
      mail-user-agent                          'mu4e-user-agent ;; or 'sendmail-user-agent
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

(spec-handling-add! mu4e-header
                    '(general
                      (:account
                       :name "Account"
                       :shortname "Account"
                       :help "which account/maildir this email belongs to"
                       :function #'(lambda (msg)
                                     (let ((maildir (replace-regexp-in-string
                                                     "\\`/?\\([^/]+\\)/.*\\'" "\\1"
                                                     (mu4e-message-field msg :maildir))))
                                       (replace-regexp-in-string
                                        "^gmail"
                                        (propertize "g" 'face 'bold-italic)
                                        maildir)
                                       '+mu4e-header--maildir-colors
                                       maildir)))
                      (:account-stripe
                       :name "Account"
                       :shortname "(A)"
                       :help "Which account/maildir this email belongs to"
                       :function
                       (lambda (msg)
                         (let ((account (replace-regexp-in-string
                                         "\\`/?\\([^/]+\\)/.*\\'" "\\1"
                                         (mu4e-message-field msg :maildir))))
                           (propertize "▌" '+mu4e-header--maildir-colors account
                                       'help-echo account))))
                      (:recipnum
                       :name "Number of recipients"
                       :shortname " (R)"
                       :help "Number of recipients for this message"
                       :function (lambda (msg)
                                   (propertize (format "%2d"
                                                       (+ (length (mu4e-message-field msg :to))
                                                          (length (mu4e-message-field msg :cc))))
                                               'face 'mu4e-footer-face)))
                      )
                    )

(spec-handling-setq! mu4e 50
                     mu4e-get-mail-command "mbsync -a"
                     mu4e-compose-signature "John"
                     mu4e-change-filenames-when-moving t
                     mu4e-use-fancy-chars nil
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
                     mu4e-headers-unread-mark    '("u" . "u")
                     )

(spec-handling-setq! auth-source 50
                     auth-source-backend-parser-functions '(auth-source-backends-parser-secrets
                                                            auth-source-backends-parser-file)
                     )

(spec-handling-setq! send-mail 50
                     message-send-mail-function #'smtpmail-send-it
                     ;; smtpmail-smtp-service         465
                     ;; smtpmail-smtp-service         587
                     ;; smtpmail-default-smtp-server  "smtp.gmail.com"
                     ;; smtpmail-smtp-server          "smtp.gmail.com"
                     ;; smtpmail-stream-type          starttls
                     )

(spec-handling-add! mail-accounts
                    `(default
                      (:name "jgrey4296"
                       :maildir "/jgrey4296"
                       :vars ((user-mail-address            . "jgrey4296@gmail.com")
                               (user-full-name               . "john grey")
                               ;; (auth-sources                 . auth-sources)
                               )
                       :mu-vars (
                                 ;; (mu4e-sent-folder             . "/sent")
                                 ;; (mu4e-drafts-folder           . "/drafts")
                                 ;; (mu4e-trash-folder            . "/trash")
                                 ;; (mu4e-refile-folder           . "/[Gmail].All Mail")
                                 )
                       :smtp-vars ((smtpmail-smtp-user           . "jgrey4296"))
                       )
                      )

                    )

(spec-handling-add! mail-accounts
                    `(plus
                      (:name "soe"
                       :maildir "/jgsoe"
                       :vars ((user-mail-address . "jgrey@soe.ucsc.edu")
                               (user-full-name . "john grey")
                               )
                       :smtp-vars ((smtpmail-smtp-user . "jgrey"))
                       )
                      )
                    )

;; (spec-handling-add! mail-accounts
;;                     `(local
;;                       ,(pcase system-type
;;                          ('darwin `(:name "local"
;;                                     :maildir ""
;;                                     :vars ((user-mail-address . "johngrey@Johns-Mac-mini.local" )
;;                                            (smtpmail-smtp-server . ,(system-name))
;;                                            )
;;                                     )
;;                                   )
;;                          ('gnu/linux  `(:name "local"
;;                                         :maildir ""
;;                                         :vars ((user-mail-address . "john@john-UM700" )
;;                                                (smtpmail-smtp-server . ,(system-name))
;;                                                )
;;                                         )
;;                                       )
;;                          )
;;                       )
;;                     )

(spec-handling-add! mail-accounts
                    `(others
                      (:name "main: johngrey"
                       :maildir "/johngrey"
                       :vars ((user-mail-address            . "johngrey4296@gmail.com")
                              (user-full-name               . "john grey")
                              )
                       :smtp-vars ((smtpmail-smtp-user           . "johngrey4296"))
                       )
                      (:name "n+1: jgnp1"
                       :maildir "/jgnp1"
                       :vars ((user-mail-address            . "jgrey.n.plus.one@gmail.com")
                               (user-full-name               . "john grey")
                               )
                       :smtp-vars ((smtpmail-smtp-user           . "jgrey.n.plus.one"))
                       )
                      (:name "old: b42"
                       :maildir "/b42"
                       :vars ((user-mail-address            . "belial42@gmail.com")
                               (user-full-name               . "john grey")
                               )
                       :smtp-vars ((smtpmail-smtp-user           . "belial42"))
                       )
                      )
)
