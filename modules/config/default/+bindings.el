;;; +bindings.el -*- lexical-binding: t; -*-


(map! :map jg-help-map
      :after jg-help-bindings
      :prefix ("s" . "Spec Handlers")
      "r" #'spec-handling-report
      "d" #'spec-handling-describe
      )


(map! :leader
      (:prefix "b"
       :n "?" #'+jg-default-debug-auto-mode
      )
      (:prefix "o"
       (:when (eq 'darwin system-type)
         :desc "Reveal in Finder"           "f"     #'+macos/reveal-in-finder
         )
       )
      )


;; OS specific fixes
(when IS-MAC
  ;; Fix MacOS shift+tab
  (define-key key-translation-map [S-iso-lefttab] [backtab])
  ;; Fix conventional OS keys in Emacs
  (map! :gi  [s-backspace] #'doom/backward-kill-to-bol-and-indent
        :gi  [s-left]      #'doom/backward-to-bol-or-indent
        :gi  [s-right]     #'doom/forward-to-last-non-comment-or-eol
        :gi  [M-backspace] #'backward-kill-word
        :gi  [M-left]      #'backward-word
        :gi  [M-right]     #'forward-word)
  )
