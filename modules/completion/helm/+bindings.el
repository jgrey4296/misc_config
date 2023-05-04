;;; completion/ivy/+bindings.el -*- lexical-binding: t; -*-

(doom-log "Setting up Completion bindings: %s" (current-time-string))
(after! (evil helm)
  (evil-make-intercept-map helm-map)
)

;;-- remap bookmarks
(map! :after counsel
      [remap bookmark-jump] #'+jg-completion-ivy-bookmark
      )
;;-- end remap bookmarks


;;-- helm

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

;; Unbinding
(map! :map helm-map
      :after helm
      "C-S-f" nil
      "C-S-n" nil
      "C-S-p" nil
      "C-S-j" nil
      "C-S-k" nil

      "C-x 5" nil
      "C-x 6" nil
      "C-x 7" nil
      "C-x 8" nil
      "C-x 9" nil
      "C-x 0" nil

      "<f1>"  nil
      "<f2>"  nil
      "<f3>"  nil
      "<f4>"  nil
      "<f5>"  nil
      "<f6>"  nil
      "<f7>"  nil
      "<f8>"  nil
      "<f9>"  nil
      "<f10>" nil
      "<f11>" nil
      "<f12>" nil

      )


;;-- end helm

;;-- leader helms/ivys
(map! :leader
      :desc "SCRATCH"                      "6" (cmd! (+jg-popup-ivy-open "*scratch*"))
      :desc "Messages"                     "0" (cmd! (+jg-popup-ivy-open "*Messages*") (when current-prefix-arg (with-current-buffer "*Messages*" (+jg-text-clear-buffer))))
      :desc "Have you Played?"      "o 1"   #'+jg-completion-rps-have-you-playeds
      )
;;-- end leader helms/ivys
