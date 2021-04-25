;;; util/jg-mail/+bindings.el -*- lexical-binding: t; -*-

(defun +jg-mail-binding-hook ()
  (message "Setting up Mail bindings")
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
