;;; util/jg-mail/+bindings.el -*- lexical-binding: t; -*-

(map! :leader
      :desc "Mail"      "9" #'mu4e
      )

(map! :after org-msg
      :map org-msg-edit-mode-map
      :localleader
      "RET" #'message-send-and-exit
      "q"   #'org-msg-edit-kill-buffer
      )

(after! mu4e (local-load! "+bind-mu4e"))
(after! rmail (local-load! "+bind-rmail"))
