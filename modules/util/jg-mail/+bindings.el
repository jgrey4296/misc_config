;;; util/jg-mail/+bindings.el -*- lexical-binding: t; -*-

(defun +jg-mail-binding-hook ()
  (message "Setting up Mail bindings: %s" (current-time-string))
  (map! :leader
        :desc "Mail"      "9" #'mu4e
        )

  (map! :map mu4e-main-mode-map
        :desc "Jump"   :n "RET" #'mu4e~headers-jump-to-maildir
        :desc "Compose" :n "c" #'mu4e-compose-new
        :desc "Quit"   :n  "q" #'mu4e-quit
        )

  (map! :map rmail-mode-map
        "j" nil
        "k" nil
        "v" nil
        "e" nil
        :n "n" #'rmail-next-undeleted-message
        :n "p" #'rmail-previous-undeleted-message
        :n "q" #'quit-window
        :n "d" #'rmail-delete-forward
        :n "Q"  #'rmail-quit
        )

  (map! :map rmail-summary-mode-map
        "q"  #'quit-window
        "Q"  #'rmail-summary-quit
        "j"  nil
        "k"  nil
        )


  (map! :map org-msg-edit-mode-map
        :localleader
        "RET" #'message-send-and-exit
        "q"   #'org-msg-edit-kill-buffer
        )
)
