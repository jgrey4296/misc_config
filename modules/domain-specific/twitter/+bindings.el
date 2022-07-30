;;; domain-specific/twitter/+bindings.el -*- lexical-binding: t; -*-

(map! :after jg-leader-bindings-loaded
      :leader
      :desc "Download Tweet Thread" "P d" #'+jg-twitter-downloader
      )

(after! evil-ex
  (message "Twitter evil ex binding: %s" (current-time-string))
  (evil-ex-define-cmd "tweet" '+jg-twitter-tweet)
  (evil-ex-define-cmd "image" '+jg-twitter-twitter-add-picture)
  (evil-ex-define-cmd "reply" '+jg-twitter-tweet-reply)
  )
