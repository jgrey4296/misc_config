;;; domain-specific/twitter/+bindings.el -*- lexical-binding: t; -*-

(defun +twitter-binding-hook ()
  (message "Twitter binding setup: %s" (current-time-string))
  (map! :leader
        :desc "Download Tweet Thread" "a d" #'+jg-tweet-downloader
        )
)

(defun +twitter-evil-ex-binding-hook ()
  (message "Twitter evil ex binding: %s" (current-time-string))
  (evil-ex-define-cmd "tweet" '+jg-twitter-tweet)
  (evil-ex-define-cmd "image" '+jg-twitter-twitter-add-picture)
  (evil-ex-define-cmd "reply" '+jg-twitter-tweet-reply)
  )
