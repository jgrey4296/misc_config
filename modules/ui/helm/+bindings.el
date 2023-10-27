;;; completion/ivy/+bindings.el -*- lexical-binding: t; -*-

(doom-log "Setting up Completion bindings: %s" (current-time-string))
(defvar jg-helm-map (make-sparse-keymap))
;; (set-keymap-parent jg-helm-map minibuffer-local-map)
;; (evil-make-intercept-map jg-helm-map)

(map! :leader
      :desc "Have you Played?"      "o 1"   #'+jg-completion-rps-have-you-playeds
      :desc "Throwaway Email"       "o 2"   (cmd! (browse-url "https://www.throwawaymail.com/"))
      )

(map! :map jg-binding-helm-map
      :desc "Minibuffer History"           "m"   #'counsel-minibuffer-history
      :desc "Shell History"                "s"   #'counsel-shell-history
      :desc "Helm Processes"               "h"   #'helm-list-emacs-process
      )


;;-- gtags
(map! :map jg-binding-jump-map
      :prefix ("g" . "gtags")
      :desc "Create Tags"           "c" #'helm-gtags-create-tags
      :desc "Find Symbol"           "y" #'helm-gtags-find-symbol
      :desc "Find Tag Other Window" "o" #'helm-gtags-find-tag-other-window
      :desc "Find Tag"              "d" #'helm-gtags-find-tag
      :desc "Find rtag"             "r" #'helm-gtags-find-rtag
      :desc "Gtags Select"          "s" #'helm-gtags-select
      :desc "Parse File"            "p" #'helm-gtags-parse-file
      :desc "Tags in func"          "i" #'helm-gtags-tags-in-this-function
      :desc "Update Tags"           "u" #'helm-gtags-update-tags
      )

;;-- end gtags

;; Movement
(map! :map jg-helm-map
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
  )
