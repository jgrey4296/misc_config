;;; domain-specific/twitter/+bindings.el -*- lexical-binding: t; -*-

(defun +twitter-binding-hook ()
  (map! :leader
        :desc "Download Tweet Thread" "a d" #'+jg-tweet-downloader
        )
)

(defun +twitter-evil-ex-binding-hook ()
  (evil-ex-define-cmd "tweet" '+jg-twitter-tweet)
  (evil-ex-define-cmd "image" '+jg-twitter-twitter-add-picture)
  )
