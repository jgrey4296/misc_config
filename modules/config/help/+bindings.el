;;; +bindings.el -*- lexical-binding: t; -*-

(defvar jg-help-map (make-keymap))
(define-prefix-command 'jg-help-map nil "jgb-help")

(map! :map jg-help-map
      "'"    #'describe-char
      "f"    #'counsel-describe-function
      "v"    #'counsel-describe-variable
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
      :desc "Emacs Manual"                "e" #'info-emacs-manual
      :desc "Emacs Manual Other Window"   "o" #'info-other-window
                                          "a" #'doom/help-autodefs
                                          "t" #'doom/toggle-profiler
                                          "V" #'doom/help-custom-variable
                                          ;; f -> fold debug
                                          ;; p -> project type
                                          ;; s -> spec handler
                                          ;; S -> spec handler

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

(map! :map free-keys-mode-map
      :after free-keys
      :desc "Change Buffer" :n "b" #'free-keys-change-buffer
      :desc "Revert Buffer" :n "g" #'revert-buffer
      :desc "Describe Mode" :n "h" #'describe-mode
      :desc "Set Prefix"    :n "p" #'free-keys-set-prefix
      :desc "Quit"          :n "q" #'quit-window
      )

(map! :map helpful-mode-map
      :n "q" #'+jg-help-switch-to-prev-helpful-or-close-window
      :n "Q" #'quit-window
      :n "=" #'helpful-update
      :n "SPC e" #'eval-last-sexp
      )

(map! :leader
      :desc "help"            "h" jg-help-map
      )
(map! :g "C-x h" jg-help-map)

(provide 'jg-help-bindings)
