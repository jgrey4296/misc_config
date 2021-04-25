;;; main/jg-org/+bindings.el -*- lexical-binding: t; -*-
(defun +jg-org-general-binding-hook ()
  (message "Setting up general access org bindings")
  (map! :leader
        "i t"    #'org-time-stamp
        "j c"    #'org-goto-calendar
        ;; AGENDA
        (:prefix ("a o" . "Org"))
        (:prefix ("a o a" . "Agenda")
         "/"   #'org-occur-in-agenda-files
         "f"   #'org-agenda-file-to-front
         "r"   #'org-remove-file
         "l"   #'org-agenda-list
         "F"   #'+jg-org-list-agenda-files
         "t"   #'org-tags-view)
        )
  )

(defun +jg-org-dired-binding-hook ()
  (message "Setting up dired org bindings")
  (map! :after dired
        :map (dired-mode-map ranger-mode-map)
        (:prefix "%"
         :desc "Mark Orgs" :n "o"     #'+jg-org-dired-select-org
         )
        :localleader
        (:prefix "m"
         :desc "Mark Orgs" "o" #'+jg-org-dired-select-org
         )
        (:prefix ("K" . "Destructive")
         :desc "Clean Marked" "c"     #'+jg-org-clean-marked-files
         :desc "Chop File Names" "C"  #'+jg-org-chop-long-files-from-dired
         :desc "Quick Compress" "Z"   #'+jg-org-quick-compress-orgs
         :desc "Fix Org Links"  "L"   #'+jg-org-dired-fix-org-links
         :desc "Remove Surplus"   "S" #'+jg-org-dired-clean-remove-surplus
         )
        )
)
