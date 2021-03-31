;;; domain-specific/twitter/+bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :desc "Download Tweet Thread" "a d" #'+jg-tweet-downloader
      )
