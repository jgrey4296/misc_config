;;; completion/ivy/+bindings.el -*- lexical-binding: t; -*-

(dlog! "Setting up Completion bindings: %s" (current-time-string))

(map! :leader
      :desc "Have you Played?"      "o 1"   #'+jg-completion-rps-have-you-playeds
      :desc "Throwaway Email"       "o 2"   (cmd! (browse-url "https://www.throwawaymail.com/"))
      ;; :desc "Helm Processes"        "r p h" #'helm-list-emacs-process
      )

(map! :map jg-helm-map
      :after helm
      "M-SPC" #'helm-next-page
      :localleader
      :desc "Save Results" "s" #'+jg-helm-save-buffer
      )

;; Movement
(map! :map jg-helm-map
      :desc "Normal State" :i "|"  #'evil-force-normal-state
      :desc "quit"         :n "|"  #'keyboard-quit

      :ni "C-u"             #'helm-delete-minibuffer-contents
      :i  "C-s"             #'helm-minibuffer-history
      "C-SPC"               #'helm-toggle-visible-mark

      [remap next-line]     #'helm-next-line
      [remap previous-line] #'helm-previous-line
      [left]                #'left-char
      [right]               #'right-char
      :i  "C-j"             #'helm-next-line
      :i  "C-k"             #'helm-previous-line

      :n "j"                #'helm-next-line
      :n "k"                #'helm-previous-line
      :n "J"                #'helm-next-page
      :n "K"                #'helm-previous-page
      :n ";"                #'+jg-helm-next-source
      :n "m"                #'helm-toggle-visible-mark
      :n "a"                #'helm-mark-all
      :n "u"                #'helm-unmark-all
      )

;; Actions
(map! :map jg-helm-map
      "RET" #'helm-maybe-exit-minibuffer
      "C-z"     #'helm-execute-persistent-action
      :ni "RET" #'helm-maybe-exit-minibuffer
      :ni "TAB" #'helm-select-action
      :n "s"    #'helm-select-action
      :n "1" (cmd! (helm-select-nth-action 0))
      :n "2" (cmd! (helm-select-nth-action 1))
      :n "3" (cmd! (helm-select-nth-action 2))
      :n "4" (cmd! (helm-select-nth-action 3))
      :n "5" (cmd! (helm-select-nth-action 4))
      :n "6" (cmd! (helm-select-nth-action 5))
      :n "7" (cmd! (helm-select-nth-action 6))
      :n "8" (cmd! (helm-select-nth-action 7))
      :n "9" (cmd! (helm-select-nth-action 8))
      )

;; localleader
(map! :map jg-helm-map
      :localleader
      :desc "Toggle Full Frame" "f" #'helm-toggle-full-frame
      )

(after! helm
  (setq helm-map jg-helm-map)
  (set-keymap-parent helm-map minibuffer-local-map)
  )
