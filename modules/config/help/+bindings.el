;;; +bindings.el -*- lexical-binding: t; -*-

(map! :map jg-help-map
      "'"    #'describe-char
      "f"    #'describe-function
      "c"    #'+jg-help-describe-class
      "v"    #'describe-variable
      "l"    #'view-lossage
      "t"    #'list-timers
      :desc "Major Mode match" "m"    (cmd! (message "Major Mode: %s" major-mode))
      :desc "Memory Report"    "M"    #'memory-report
      "!"   #'doom-debug-mode
      "DEL" #'free-keys
      )

(map! :map jg-help-map
      :prefix ("e" . "Edit")
      :desc "Bindings" "b" #'+jg-help-edit-bindings
      :desc "Vars"     "v" #'+jg-help-edit-vars
      :desc "Config"   "c" #'+jg-help-edit-config
      :desc "Spec-Defs" "s" #'+jg-help-edit-spec-defs
      )

;;-- docs
(map! :map jg-help-map
      :prefix ("d" . "docs")
      :desc "Man"                         "m" #'+jg-help-man ;; #'man  ;; #'+man-or-woman
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
      "e" #'+jg-help-edit-bindings
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
      ;; s -> spec-handlers
      )

;;-- end reloading

;;-- packages
(map! :map jg-help-map
      :prefix ("p" . "Packages")
      :desc "Sraight Repos"  "1" (cmd! (find-file (expand-file-name "straight/repos" straight-base-dir)))
      "c" #'doom/help-package-config
      "d" #'doom/goto-private-packages-file
      "h" #'doom/help-package-homepage
      "p" #'doom/help-packages
      "l" #'doom/help-search-load-path
      "L" #'doom/help-search-loaded-files
      "m" #'doom/help-modules
      "V" #'doom/version
      "r" #'straight-rebuild-package
      :desc "Emacs Version Config"               "v"   #'+jg-help-system-config
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
      :desc "Local Variables" "b l"   #'+jg-help-list-buffer-locals
      )
(map! :g "C-x h" jg-help-map)

(provide 'jg-help-bindings)
