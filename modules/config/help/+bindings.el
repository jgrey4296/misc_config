;;; +bindings.el -*- lexical-binding: t; -*-

(setq jg-binding-help-map (make-keymap))

(map! :map jg-binding-help-map
      :desc "Interactive Code Reminder" "1" #'+jg-bindings-evil-interactive-reminder
      :desc "Regexp Syntax"             "2" (cmd! (info "(elisp) Syntax of Regexps"))

      "'"    #'describe-char
      "f"    #'describe-function
      "v"    #'describe-variable

      )

;;-- docs
(map! :map jg-binding-help-map
      :prefix ("d" . "docs")
      "!"   #'+jg-bindings-system-config
      "m"   #'man  ;; #'+default/man-or-woman
      "RET" #'info-emacs-manual
      "o"   #'info-other-window
      )
;;-- end docs

;;-- code
(map! :map jg-binding-help-map
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
(map! :map jg-binding-help-map
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
(map! :map jg-binding-help-map
      :prefix ("b" . "Bindings")
      "b"   #'describe-bindings
      "f" #'which-key-show-full-keymap
      "i" #'which-key-show-minor-mode-keymap
      "k" #'which-key-show-keymap
      "m" #'which-key-show-major-mode
      "t" #'+jg-which-key-show-top-level
      "c" #'describe-key
      )

;;-- end bindings

;;-- reloading
(map! :map jg-binding-help-map
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
(map! :map jg-binding-help-map
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
(map! :map jg-binding-help-map
      :prefix ("p" . "Packages")
       "c"  #'doom/help-package-config
       "d"  #'doom/goto-private-packages-file
       "h"  #'doom/help-package-homepage
       "p"  #'doom/help-packages
      )
;;-- end packages

;;-- modules
(map! :map jg-binding-help-map
      :desc "Module Ivy" "m" #'+jg-help-modules-ivy

      )

;;-- end modules

(map! :leader
      :desc "help" "h" jg-binding-help-map
      )
(map! :g "C-x h" jg-binding-help-map)

(defun +jg-help-buffer-list (curr)
  (cl-remove-if-not #'(lambda (buf)
                        (with-current-buffer buf
                          (and (not (eq curr buf))
                               (derived-mode-p 'helpful-mode))
                          ))
                    (buffer-list))
  )

(defun +jg-help-switch-to-prev-helpful-or-close-window ()
  (interactive)
  (if-let ((next-helpful (car-safe (+jg-help-buffer-list (current-buffer))))
           (curr (current-buffer))
           )
      (progn (switch-to-buffer next-helpful t t)
             (kill-buffer curr))
    (+popup/quit-window)
    )
  )


(map! :map helpful-mode-map
      :n "q" #'+jg-help-switch-to-prev-helpful-or-close-window
      )
(provide 'jg-help-bindings)
