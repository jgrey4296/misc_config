;;; main/jg-org/+bindings.el -*- lexical-binding: t; -*-


(map! :leader
       "i t"    #'org-time-stamp
       "j c"    #'org-goto-calendar
      ;; AGENDA
      (:prefix ("a o a" . "Agenda")
        "/"   #'org-occur-in-agenda-files
        "f"   #'org-agenda-file-to-front
        "r"   #'org-remove-file
        "l"   #'org-agenda-list
        "F"   #'+jg-org-list-agenda-files
        "t"   #'org-tags-view)
      )
;;; Paren Control
(map! :map org-mode-map
      "C-c [" nil
      "C-c ]" nil
      )
;;; Personal
(map! :map org-mode-map
      :leader
      :desc "Toggle Links" "t l"   'org-toggle-link-display)
(map! :map org-mode-map
      :desc "Next Link"       :n "] p" #'org-next-link
      :desc "Prev Link"       :n "[ p" #'org-previous-link
      (:prefix "g"
       :desc "Forward Heading" :n "j" #'org-forward-heading-same-level
       :desc "Back Heading"    :n "k" #'org-backward-heading-same-level
       :desc "Headings Helm"   :n "h" #'helm-org-in-buffer-headings)
      )
(map! :map org-mode-map
      :localleader
      "i" nil
      :desc "Refile" "R" #'+jg-org-refile-subtree
      (:prefix ("f". "Format")
       :desc "Fix Drawers"     :n "d"  #'+jg-org-fix-properties-drawers
       :desc "Clean Org"          "c"  #'+jg-org-clean-org
       :desc "Wrap Numbers"        "w" #'+jg-org-wrap-numbers
       :desc "Wrap non-link urls"  "L" #'+jg-org-wrap-non-link-urls
       :desc "Remove Duplicates"   "D" #'+jg-org-remove-duplicates)
      ;; TODO refine this Codeblocks
      (:prefix "."
       :desc "Edit Codeblock " "e"     #'org-edit-src-code
       :desc "Exec Codeblock"  "E"     #'org-babel-execute-src-block)
      ;; Links
      (:prefix ("l" . "Links")
       ;; "o"   #'+jg-org-open_link_in_buffer
       ;; "O"   #'+jg-org-open_link_externally
       "n"      #'+jg-org-change_link_name
       )
      ;; Insertion
      (:prefix ("i" . "Insert")
       :desc "Insert Heading Trio" "t" #'+jg-org-insert-heading-trio
       :desc "Insert Subheading" "h"   #'org-insert-subheading
       :desc "Insert Drawer" "d"       #'org-insert-drawer)
      )

(map! :after dired
      :map (dired-mode-map ranger-mode-map)
      :localleader
      (:prefix ("K" . "Destructive")
       :desc "Clean Marked" "c"              #'+jg-org-clean-marked-files
       :desc "Chop File Names" "C"           #'+jg-org-chop-long-files-from-dired
       :desc "Quick Compress" "Z"            #'+jg-org-quick-compress-orgs
       :desc "Fix Org Links"  "L"            #'+jg-org-dired-fix-org-links
       :desc "Remove Duplicates Tweets" "D"  #'+jg-org-dired-remove-duplicate-tweets
       :desc "Remove Surplus Headings"   "H" #'+jg-org-dired-clean-remove-surplus-headings
       :desc "Sort Headings" "S"             #'+jg-org-dired-clean-sort-headings
       )
      )
