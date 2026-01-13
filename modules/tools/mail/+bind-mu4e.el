;;; +bind-mu4e.el -*- lexical-binding: t; no-byte-compile: t; -*-


(evil-make-intercept-map mu4e-main-mode-map)
(map! :map mu4e-main-mode-map
      :desc "Jump"    "RET" #'mu4e~headers-jump-to-maildir
      :desc "Compose" "c"   #'mu4e-compose-new
      :desc "Quit"    "q"   #'mu4e-quit
      :desc "test"    "a"   (cmd! (message "blah"))
      "j" nil
      "k" nil
      )
(map! :map mu4e-view-mode-map
      :ne "A" #'+mu4e-view-select-mime-part-action
      :ne "p" #'mu4e-view-save-attachments
      :ne "o" #'+mu4e-view-open-attachment
      )

(map! :map mu4e-compose-mode-map
      :localleader
      :desc "Switch Context" ";" #'mu4e-context-switch
      :map mu4e-compose-mode-map
      :desc "send and exit" "s" #'message-send-and-exit
      :desc "kill buffer"   "d" #'message-kill-buffer
      :desc "save draft"    "S" #'message-dont-send
      :desc "attach"        "a" #'+mu4e/attach-files)

(map! :map mu4e-headers-mode-map
      :v "*" #'mu4e-headers-mark-for-something
      :v "!" #'mu4e-headers-mark-for-read
      :v "?" #'mu4e-headers-mark-for-unread
      :v "u" #'mu4e-headers-mark-for-unmark
      )
(map! :leader
      :prefix ("o" . "Open")
      :desc "Compose Email"                "e" #'compose-mail
      )



;;; +bind-mu4e.el ends here
