;; +defs.el -*- lexical-binding: t; -*-

(defvar +mu4e-min-header-frame-width 120 "Minimum reasonable with for the header view.")

;; Add a column to display what email account the email belongs to, and an account color stripe column

(defvar +mu4e-header--maildir-colors nil)

(defvar +mu4e-main-bullet "*"
  "Prefix to use instead of \"	*\" in the mu4e main view.
This is enacted by `+mu4e~main-action-str-prettier-a' and `+mu4e~main-keyval-str-prettier-a'.")

(defvar +mu4e-gmail-accounts nil
  "Gmail accounts that do not contain \"gmail\" in address and maildir.

An alist of Gmail addresses of the format \((\"username@domain.com\" . \"account-maildir\"))
to which Gmail integrations (behind the `+gmail' flag of the `mu4e' module) should be applied.

See `+mu4e-msg-gmail-p' and `mu4e-sent-messages-behavior'.")

(defvar +mu4e--last-invalid-gmail-action 0)

(defvar +mu4e-backend 'mbsync
  "Which backend to use. Can either be offlineimap, mbsync or nil (manual).")

(defvar +mu4e-personal-addresses 'nil
  "Alternative to mu4e-personal-addresses that can be set for each account (mu4e context).")

(defvar +mu4e-header-info-custom
      '((:account . (:name "Account"
                     :shortname "Account"
                     :help "which account/maildir this email belongs to"
                     :function #'(lambda (msg)
                                   (let ((maildir (replace-regexp-in-string
                                                   "\\`/?\\([^/]+\\)/.*\\'" "\\1"
                                                   (mu4e-message-field msg :maildir))))
                                     (+mu4e-colorize-str (replace-regexp-in-string
                                                          "^gmail"
                                                          (propertize "g" 'face 'bold-italic)
                                                          maildir)
                                                         '+mu4e-header--maildir-colors
                                                         maildir)))))
        (:account-stripe . (:name "Account"
                            :shortname "(A)"
                            :help "Which account/maildir this email belongs to"
                            :function
                            (lambda (msg)
                              (let ((account (replace-regexp-in-string
                                              "\\`/?\\([^/]+\\)/.*\\'" "\\1"
                                              (mu4e-message-field msg :maildir))))
                                (propertize
                                 (+mu4e-colorize-str "▌" '+mu4e-header--maildir-colors account)
                                 'help-echo account)))))
        (:recipnum . (:name "Number of recipients"
                      :shortname " (R)"
                      :help "Number of recipients for this message"
                      :function (lambda (msg)
                                  (propertize (format "%2d"
                                                      (+ (length (mu4e-message-field msg :to))
                                                         (length (mu4e-message-field msg :cc))))
                                              'face 'mu4e-footer-face)))))
      )
