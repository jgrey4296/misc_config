;;; +bindings.el -*- lexical-binding: t; -*-

(define-prefix-command 'jg-help-map nil "jgb-help")

(map! :leader
      :desc "help"            "h" jg-help-map
      )

(map! :g "C-x h" jg-help-map)

(map! :map jg-help-map
      "'"    #'describe-char
      "f"    #'counsel-describe-function
      "v"    #'counsel-describe-variable
      "V"    #'+jg-help-describe-env-var
      "l"    #'view-lossage
      "t"    #'list-timers
      :desc "Major Mode match" "m"    (cmd! (message "Major Mode: %s" major-mode))
      :desc "Memory Report"    "M"    #'memory-report
      "!"   #'doom-debug-mode
      "DEL" #'free-keys

      )

;;-- debug
(map! :map jg-help-map
      :prefix ("D" . "Debug")
      "f" #'+jg-default-debug-auto-mode
      "s" #'+jg-snippets-insert-debug
      "l" #'+jg-lookup-debug-settings
      "f" #'+jg-fold/debug
      "i" #'+jg-ide-debug-lsp
      "d" #'+jg-snippets-debug-dirs
      "t" #'+file-templates/debug
      "p" #'+popup/diagnose
      "c" #'flycheck-describe-checker
      "w" #'+jg-projects-detect-type
      )

;;-- end debug

;;-- docs
(map! :map jg-help-map
      :prefix ("d" . "docs")
      :desc "Info Manual"                 "i" #'info-display-manual
      ;; :desc "Emacs Manual"                "e" #'info-emacs-manual
      ;; :desc "Emacs Manual Other Window"   "o" #'info-other-window
      "a" #'doom/help-autodefs
      "t" #'doom/toggle-profiler
      ;; p -> project type
      "1"   #'view-hello-file

      (:prefix ("h" . "Character sets")
               "l" #'list-character-sets
               "d" #'describe-character-set
               )

      (:prefix ("l" . "Language Environment")
               "s"   #'set-language-environment
               "d"   #'describe-language-environment
               )
      (:prefix ("c" . "Coding System")
               "d"   #'describe-coding-system
               "s"   #'set-terminal-coding-system
               "l"   #'list-coding-systems
               )

      (:prefix ("g" . "Input Method")
               "l"   #'list-input-methods
               "d"   #'describe-input-method
               "s"   #'set-input-method
               )
    )

;;-- end docs

;;-- ui
(map! :map jg-help-map
      :prefix ("u" . "UI")
      "b" #'describe-bindings
      "C" #'describe-coding-system
      "c" #'helm-colors
      "f" #'describe-face
      "L" #'describe-language-environment
      "l" #'load-theme
      "t"   #'doom/reload-theme
      :desc "Active Minor Modes" "M" #'doom/describe-active-minor-mode
      "m" #'+jg-help-reset-major-mode
      )
;;-- end ui

;;-- bindings
(map! :map jg-help-map
      :prefix ("b" . "Bindings")
      "a" #'+jg-help-describe-active-maps
      "b" #'describe-bindings
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
      :desc "Reload"           "r"   #'doom/reload
      :desc "Reload Theme"     "t"   #'doom/reload-theme
      :desc "Reload Packages"  "p"   #'doom/reload-packages
      :desc "Reload Font"      "f"   #'doom/reload-font
      :desc "Reload env"       "e"   #'doom/reload-env
      ;; s -> spec-handlers
      )

;;-- end reloading

;;-- packages
(map! :map jg-help-map
      :prefix ("p" . "Packages")
      :desc "Sraight Repos"                      "1" (cmd! (find-file (expand-file-name "straight/repos" straight-base-dir)))
      :desc "Package Config"                     "c" #'doom/help-package-config
      :desc "Goto private packages"              "d" #'doom/goto-private-packages-file
      :desc "Goto package homepage"              "h" #'doom/help-package-homepage
      :desc "Open Help for package"              "p" #'doom/help-packages
      :desc "Search Load Path"                   "l" #'doom/help-search-load-path
      :desc "Search Loaded Files"                "L" #'doom/help-search-loaded-files
      :desc "Modules Help"                       "m" #'doom/help-modules
      :desc "Versions"                           "V" #'doom/version
      :desc "Rebuild Package"                    "r" #'straight-rebuild-package
      )
;;-- end packages

;;-- free-keys
(map! :map free-keys-mode-map
      :after free-keys
      :desc "Change Buffer" :n "b" #'free-keys-change-buffer
      :desc "Revert Buffer" :n "g" #'revert-buffer
      :desc "Describe Mode" :n "h" #'describe-mode
      :desc "Set Prefix"    :n "p" #'free-keys-set-prefix
      :desc "Quit"          :n "q" #'quit-window
      )

;;-- end free-keys

;;-- helpful
(map! :map helpful-mode-map
      :n "q" #'+jg-help-switch-to-prev-helpful-or-close-window
      :n "Q" #'quit-window
      :n "=" #'helpful-update
      :n "SPC e" #'eval-last-sexp
      )

;;-- end helpful

;;-- info
(map! :map jg-info-map
    :n "]" #'Info-forward-node
    :n "[" #'Info-backward-node
    :n "h" #'Info-up
    :n "H" #'Info-top-node
    :n "l" #'Info-next

    :n "s n" #'Info-goto-node
    :n "s N" #'Info-goto-node-web

    :n "q" #'quit-window
    :n "DEL" #'Info-toc
    :n "RET" #'Info-follow-nearest-node

    )

;;-- end info

(setq help-mode-map (make-sparse-keymap)
      Info-mode-map jg-info-map
      help-map jg-help-map
      )

(provide 'jg-help-bindings)
