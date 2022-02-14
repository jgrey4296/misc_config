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

(map! ;:after rmailsum
      :map rmail-summary-mode-map
      [menu-bar] nil
      "j"  nil
      "k"  nil
      "d"  nil
      "SPC" nil
      "q"  #'quit-window
      "Q"  #'rmail-summary-quit
      "d"  #'rmail-summary-delete-forward
      "\\" #'rmail-summary-end-of-message
      "/"  #'rmail-search
      :localleader
      :prefix ("s" . "Sort")
      :desc "By Recipient" "r" #'rmail-summary-sort-by-recipient
      :desc "By Author" "a" #'rmail-summary-sort-by-author
      :desc "By Correspondent" "c" #'rmail-summary-sort-by-correspondent
      :desc "By Date" "d" #'rmail-summary-sort-by-date
      :desc "By Lines" "l" #'rmail-summary-sort-by-lines
      :desc "By Subject" "s" #'rmail-summary-sort-by-subject
)

(map! :after rmail
      :map rmail-mode-map
      "j" nil
      "k" nil
      "v" nil
      "e" nil
      "q" nil
      "/" nil
      "n" #'rmail-next-undeleted-message
      "p" #'rmail-previous-undeleted-message
      "q" #'quit-window
      "Q" #'rmail-quit
      "d" #'rmail-delete-forward
      "\\" #'rmail-end-of-message
      )
