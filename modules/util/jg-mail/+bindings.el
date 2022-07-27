;;; util/jg-mail/+bindings.el -*- lexical-binding: t; -*-

(map! :after jg-leader-bindings-loaded
      :leader
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

  (map! :after jg-leader-bindings-loaded
        :leader
        :prefix ("o" . "Open")
        :desc "Compose Email"                "e" #'compose-mail
        )
  )

(map! :after org-msg
      :map org-msg-edit-mode-map
      :localleader
      "RET" #'message-send-and-exit
      "q"   #'org-msg-edit-kill-buffer
      )


(after! rmail
  (setq rmail-summary-mode-map (make-sparse-keymap)
        rmail-mode-map (make-sparse-keymap))

  (evil-make-intercept-map rmail-mode-map)
  (evil-make-intercept-map rmail-summary-mode-map)
  (map! :map rmail-summary-mode-map
   "RET" #'rmail-summary-goto-msg
   "!"   #'rmail-summary-expunge-and-save
   "/"   #'rmail-summary-search
   "L"   #'+jg-mail-summary-unlabel-by-regex
   "Q"   #'rmail-summary-quit
   "\\"  #'rmail-summary-end-of-message
   "c"   #'rmail-summary-mail
   "d"   #'rmail-summary-delete-forward
   "u"   #'rmail-summary-undelete
   "U"   #'rmail-summary-undelete-many
   "l"   #'+jg-mail-summary-label-by-regex
   "q"   #'quit-window
   "r"   #'rmail-summary-reply
   "h"  (cmd! (rmail-summary)
              (beginning-of-buffer))
   (:prefix ("D" . "Delete")
    :desc "Delete Msg" "m"                #'+jg-mail-summary-delete-msg
    :desc "Delete All" "a"                #'+jg-mail-summary-delete-all
    :desc "Unmark deletions by regex" "u" #'+jg-mail-summary-undelete-by-regex
    :desc "Delete by Regex" "r"           #'+jg-mail-summary-delete-by-regex
    )
   (:prefix ("s" . "Sort")
    :desc "By Recipient     " "r" #'rmail-summary-sort-by-recipient
    :desc "By Author        " "a" #'rmail-summary-sort-by-author
    :desc "By Correspondent " "c" #'rmail-summary-sort-by-correspondent
    :desc "By Date          " "d" #'rmail-summary-sort-by-date
    :desc "By Lines         " "l" #'rmail-summary-sort-by-lines
    :desc "By Subject       " "s" #'rmail-summary-sort-by-subject
    :desc "By Labels        " "b" #'rmail-summary-by-labels
    )

   )
  (map! :map rmail-mode-map
        "!" #'rmail-expunge-and-save
        "L" #'rmail-kill-label
        "Q" #'rmail-quit
        "\\" #'rmail-end-of-message
        "c" #'rmail-mail
        "d" #'rmail-delete-forward
        "d" #'rmail-delete-forward
        "h" (cmd! (rmail-summary) (beginning-of-buffer))
        "l" #'rmail-add-label
        "n" #'rmail-next-undeleted-message
        "p" #'rmail-previous-message
        "q" #'quit-window
        "r" #'rmail-reply
        "u" #'rmail-undelete-previous-message
        "|" #'rmail-beginning-of-message
        )
  )
