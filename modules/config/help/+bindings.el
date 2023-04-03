;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map jg-help-map
      :desc "Interactive Code Reminder" "1" #'+jg-help-evil-interactive-reminder
      :desc "Regexp Syntax"             "2" (cmd! (info "(elisp) Syntax of Regexps"))

      "'"    #'describe-char
      "f"    #'describe-function
      "v"    #'describe-variable
      "l"    #'view-lossage

      "DEL" #'free-keys
      )

(map! :map free-keys-mode-map
      :after free-keys
      :desc "Change Buffer" :n "b" #'free-keys-change-buffer
      :desc "Revert Buffer" :n "g" #'revert-buffer
      :desc "Describe Mode" :n "h" #'describe-mode
      :desc "Set Prefix"    :n "p" #'free-keys-set-prefix
      :desc "Quit"          :n "q" #'quit-window
      )

;;-- docs
(map! :map jg-help-map
      :prefix ("d" . "docs")
      "!"   #'+jg-help-system-config
      "m"   #'+jg-help-man ;; #'man  ;; #'+man-or-woman
      "RET" #'info-emacs-manual
      "o"   #'info-other-window
      )
;;-- end docs

;;-- code
(map! :map jg-help-map
      :prefix ("c" . "code")
      "a" #'doom/help-autodefs
      "f" #'describe-function
      "P" #'find-library
      "t" #'doom/toggle-profiler
      "v" #'describe-variable
      "V" #'doom/help-custom-variable
      "x" #'doom/sandbox
      )

;;-- end code

;;-- ui
(map! :map jg-help-map
      :prefix ("u" . "UI")
      "b" #'describe-bindings
      "c" #'describe-coding-system
      "f" #'describe-face
      "L" #'describe-language-environment
      "l" #'load-theme
      "M" #'doom/describe-active-minor-mode
      )
;;-- end ui

;;-- bindings
(map! :map jg-help-map
      :prefix ("b" . "Bindings")
      "b"   #'describe-bindings
      "f" #'which-key-show-full-keymap
      "i" #'which-key-show-minor-mode-keymap
      "k" #'which-key-show-keymap
      "m" #'which-key-show-major-mode
      "t" #'+jg-help-top-level-keymap
      "c" #'describe-key
      )

;;-- end bindings

;;-- reloading
(map! :map jg-help-map
      ;; replacement keybinds
      :prefix ("r" . "Reload")
      "r"   #'doom/reload
      "t"   #'doom/reload-theme
      "p"   #'doom/reload-packages
      "f"   #'doom/reload-font
      "e"   #'doom/reload-env
      )

;;-- end reloading

;;-- doom
(map! :map jg-help-map
      :prefix ("D" . "Doom")
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
      "S"   #'doom/help-search
      "s"   #'doom/help-search-headings
      "v"   #'doom/version
      )
;;-- end doom

;;-- packages
(map! :map jg-help-map
      :prefix ("p" . "Packages")
       "c"  #'doom/help-package-config
       "d"  #'doom/goto-private-packages-file
       "h"  #'doom/help-package-homepage
       "p"  #'doom/help-packages
      )
;;-- end packages

;;-- modules
(map! :map jg-help-map
      :desc "Module Ivy" "m" #'+jg-help-modules-ivy

      )

;;-- end modules

(map! :leader
      :desc "help" "h" jg-help-map
      )
(map! :g "C-x h" jg-help-map)

(provide 'jg-help-bindings)
