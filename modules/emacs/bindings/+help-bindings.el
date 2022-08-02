;;; +help-bindings.el -*- lexical-binding: t; -*-

(setq jg-binding-help-map (copy-keymap help-map))

(map! :map jg-binding-help-map
      :desc "Interactive Code Reminder" "1" #'+jg-bindings-evil-interactive-reminder
      :desc "Regexp Syntax"             "2" (cmd! (info "(elisp) Syntax of Regexps"))
      "4" #'info-other-window

      ;; new keybinds
      "'"    #'describe-char
      "u"    #'doom/help-autodefs
      "E"    #'doom/sandbox
      "M"    #'doom/describe-active-minor-mode
      "O"    #'+lookup/online
      "T"    #'doom/toggle-profiler
      "V"    #'doom/help-custom-variable
      "W"    #'+default/man-or-woman
      ;; "C-k"  #'describe-key-briefly
      "C-l"  #'describe-language-environment
      "C-m"  #'info-emacs-manual


      ;; replaces `apropos-command'
      "a"    #'apropos
      "A"    #'apropos-documentation
      ;; replaces `describe-copying' b/c not useful
      "C-c"  #'describe-coding-system
      ;; replaces `Info-got-emacs-command-node' b/c redundant w/ `Info-goto-node'
      "F"    #'describe-face
      ;; replaces `view-emacs-news' b/c it's on C-n too
      "n"    #'doom/help-news
      ;; replaces `help-with-tutorial', b/c it's less useful than `load-theme'
      "t"    #'load-theme
      ;; replaces `finder-by-keyword' b/c not useful
      "p"    #'doom/help-packages
      ;; replaces `describe-package' b/c redundant w/ `doom/help-packages'
      "P"    #'find-library
      )

(map! :map jg-binding-help-map
      ;; replacement keybinds
      ;; replaces `info-emacs-manual' b/c it's on C-m now
      "r" nil
      :prefix ("r" . "Reload")
       "r"   #'doom/reload
       "t"   #'doom/reload-theme
       "p"   #'doom/reload-packages
       "f"   #'doom/reload-font
       "e"   #'doom/reload-env
       )

(map! :map jg-binding-help-map
      ;; make `describe-bindings' available under the b prefix which it previously
      ;; occupied. Add more binding related commands under that prefix as well
      "b" nil
      :prefix ("b" . "Bindings")
       "b"   #'describe-bindings
       "i"   #'which-key-show-minor-mode-keymap
       "m"   #'which-key-show-major-mode
       "t"   #'which-key-show-top-level
       "f"   #'which-key-show-full-keymap
       "k"   #'which-key-show-keymap
       "t"   #'+jg-which-key-show-top-level
       )

(map! :map jg-binding-help-map
      ;; replaces `apropos-documentation' b/c `apropos' covers this
      "d" nil
      :prefix ("d" . "Doom")
      "b"   #'doom/report-bug
      "c"   #'doom/goto-private-config-file
      "C"   #'doom/goto-private-init-file
      "d"   #'doom-debug-mode
      "f"   #'doom/help-faq
      "h"   #'doom/help
      "l"   #'doom/help-search-load-path
      "L"   #'doom/help-search-loaded-files
      "m"   #'doom/help-modules
      "n"   #'doom/help-news
      "N"   #'doom/help-search-news

      (:prefix ("p" . "Packages")
       "c"  #'doom/help-package-config
       "d"  #'doom/goto-private-packages-file
       "h"  #'doom/help-package-homepage
       "p"  #'doom/help-packages
       )
      "s"   #'doom/help-search-headings
      "S"   #'doom/help-search
      "t"   #'doom/toggle-profiler
      "u"   #'doom/help-autodefs
      "v"   #'doom/version
      "x"   #'doom/sandbox
)

(after! jg-leader-bindings-loaded
  (setq help-map jg-binding-help-map)
  (map! :leader
        :desc "help"                  "h"    help-map
        )
  (map! :g "C-x h" help-map)
  )


(provide 'jg-help-bindings)
