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
   "Q"   #'+jg-mail-quit-rmail
   "\\"  #'rmail-summary-end-of-message
   "c"   #'rmail-summary-mail
   "d"   #'+jg-mail-summary-delete-msg
   "u"   #'rmail-summary-undelete
   "l"   #'+jg-mail-summary-label-by-regexp
   "q"   #'quit-window
   "r"   #'rmail-summary-reply
   "h"  (cmd! (rmail-summary)
              (beginning-of-buffer))
   (:prefix ("L" . "Labelling")
    :desc "Remove Label"          "u" #'rmail-summary-kill-label
    :desc "Delete Label from all" "U" #'+jg-mail-summary-remove-label
    :desc "Unlabel by regexp"     "r" #'+jg-mail-summary-unlabel-by-regexp
    )
   (:prefix ("D" . "Delete")
    :desc "Delete All"      "a" #'+jg-mail-summary-delete-all
    :desc "Delete by Regex" "r" #'+jg-mail-summary-delete-by-regexp
    :desc "Delete by Label" "l" #'+jg-mail-summary-delete-by-label
    :desc "Delete by Date"  "d" #'+jg-mail-summary-delete-older-than
    )
   (:prefix ("U" . "Undelete")
    :desc "Undelete All"       "a" #'+jg-mail-summary-undelete-all
    :desc "Undelete by Regex"  "r" #'+jg-mail-summary-undelete-by-regexp
    :desc "Undelete by Label"  "l" #'+jg-mail-summary-undelete-by-label
    )
   (:prefix ("s" . "Sort")
    :desc "By Recipient     " "r" #'rmail-summary-sort-by-recipient
    :desc "By Author        " "a" #'rmail-summary-sort-by-author
    :desc "By Correspondent " "c" #'rmail-summary-sort-by-correspondent
    :desc "By Date          " "d" #'rmail-summary-sort-by-date
    :desc "By Lines         " "l" #'rmail-summary-sort-by-lines
    :desc "By Subject       " "s" #'rmail-summary-sort-by-subject
    :desc "By Labels        " "b" #'+jg-mail-summary-by-labels
    )
   )

  (map! :map rmail-mode-map
        "!" #'rmail-expunge-and-save
        "L" #'rmail-kill-label
        "Q" #'+jg-mail-quit-rmail
        "\\" #'rmail-end-of-message
        "c" #'rmail-mail
        "d" #'rmail-delete-forward
        "d" #'rmail-delete-forward
        "h" (cmd! (rmail-summary) (beginning-of-buffer))
        "l" #'rmail-add-label
        "N" #'rmail-next-undeleted-message
        "P" #'rmail-previous-message
        "q" #'quit-window
        "r" #'rmail-reply
        "u" #'rmail-undelete-previous-message
        "|" #'rmail-beginning-of-message
        )
  )
