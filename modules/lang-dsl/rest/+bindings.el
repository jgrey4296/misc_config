;; -*- mode:emacs-lisp; lexical-binding: t; -*-

(map! :map restclient-mode-map
      :n [return] #'+rest/dwim-at-point
      :n "za" #'restclient-toggle-body-visibility
      :n "zm" #'+rest/fold-all
      :n "zr" #'outline-show-all

      :localleader
      "e" #'restclient-http-send-current
      "E" #'restclient-http-send-current-raw
      "c" #'restclient-copy-curl-command
      )
