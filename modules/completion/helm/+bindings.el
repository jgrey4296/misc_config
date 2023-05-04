;;; completion/ivy/+bindings.el -*- lexical-binding: t; -*-

(doom-log "Setting up Completion bindings: %s" (current-time-string))
(setq helm-map (make-sparse-keymap))
(set-keymap-parent helm-map minibuffer-local-map)
;; (evil-make-intercept-map helm-map)

(map! :leader
      :desc "SCRATCH"                      "6" (cmd! (+jg-popup-ivy-open "*scratch*"))
      :desc "Messages"                     "0" (cmd! (+jg-popup-ivy-open "*Messages*") (when current-prefix-arg (with-current-buffer "*Messages*" (+jg-text-clear-buffer))))
      :desc "Have you Played?"      "o 1"   #'+jg-completion-rps-have-you-playeds
      )

(map! :map jg-binding-helm-map
      :desc "Minibuffer History"           "m"   #'counsel-minibuffer-history
      :desc "Shell History"                "s"   #'counsel-shell-history
      :desc "Helm Processes"               "h"   #'helm-list-emacs-process
      )

;;-- remap bookmarks
(map!
 [remap bookmark-jump] #'+jg-completion-ivy-bookmark
 )
;;-- end remap bookmarks

;; Movement
(map! :map helm-map
      :after helm
      :desc "quit" :n "|"  #'evil-force-normal-state

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
      :n ";"                #'helm-toggle-visible-mark
      :n "a"                #'helm-mark-all
      :n "u"                #'helm-unmark-all
      )

;; Actions
(map! :map helm-map
      :after helm
      :ni [ret] #'helm-maybe-exit-minibuffer
      :ni [tab] #'helm-select-action
      "C-z"     #'helm-execute-persistent-action
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
(map! :map helm-map
      :after helm
      :localleader
      :desc "Toggle Full Frame" "f" #'helm-toggle-full-frame
      )
