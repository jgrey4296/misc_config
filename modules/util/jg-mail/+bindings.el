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


  (map! :map org-msg-edit-mode-map
        :localleader
        "RET" #'message-send-and-exit
        "q"   #'org-msg-edit-kill-buffer
        )
)

(defun +jg-mail-rmail-binding-hook ()
  (map! :map rmail-mode-map
        "j" nil
        "k" nil
        "v" nil
        "e" nil
        "q" nil
        "q" #'quit-window
        :n "n" #'rmail-next-undeleted-message
        :n "p" #'rmail-previous-undeleted-message
        :n "q" #'quit-window
        :n "Q" #'rmail-quit
        :n "d" #'rmail-delete-forward
        )
)


(defun +jg-mail-rmail-summary-binding-hook ()
  (map! :map rmail-summary-mode-map
        "j"  nil
        "k"  nil
        "q"  #'quit-window
        "Q"  #'rmail-summary-quit

        :n "q"  #'quit-window
        :n "Q"  #'rmail-summary-quit
        )
)
