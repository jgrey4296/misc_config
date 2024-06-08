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
