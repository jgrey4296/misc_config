;;; util/jg-mail/+bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :desc "Mail"      "9" #'mu4e
      )

(after! mu4e
  (evil-make-intercept-map mu4e-main-mode-map)
  (map! :map mu4e-main-mode-map
        :desc "Jump"    "RET" #'mu4e~headers-jump-to-maildir
        :desc "Compose" "c"   #'mu4e-compose-new
        :desc "Quit"    "q"   #'mu4e-quit
        :desc "test"    "a"   (cmd! (message "blah"))
        "j" nil
        "k" nil
        )

  (map! :map mu4e-compose-mode-map
        :localleader
        :desc "Switch Context" ";" #'mu4e-context-switch
        )

  (map! :leader
        :prefix ("o" . "Open")
        :desc "Compose Email"                "e" #'compose-mail
        )
  )

(after! org-msg
  (map! :map org-msg-edit-mode-map
        :localleader
        "RET" #'message-send-and-exit
        "q"   #'org-msg-edit-kill-buffer
        )
  )
