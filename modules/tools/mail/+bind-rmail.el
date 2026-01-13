;;; +bind-rmail.el -*- lexical-binding: t; no-byte-compile: t; -*-

(def-named-keymap! rmail-summary-mode-map :sparse t)

(def-named-kedmay! rmail-mode-map :sparse t)

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

;;; +bind-rmail.el ends here
