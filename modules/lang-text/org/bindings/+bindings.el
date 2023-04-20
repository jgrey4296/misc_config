;;; main/jg-org/+bindings.el -*- lexical-binding: t; -*-
(message "Setting up general access org bindings: %s" (current-time-string))

;;-- leader map
(map! :leader
      :desc "Insert Timestamp"  "i t"   #'org-time-stamp
      :desc "Jump to Calendar"  "j c"   #'org-goto-calendar
      :desc "Toggle Links"      "t l"   #'org-toggle-link-display
      )
(map! :leader
      :prefix ("o" . "Org")
      :prefix ("a" . "Org Agenda")
      :desc "Agenda"                "a"          #'org-agenda
      :desc "Todo list"             "t"          #'org-todo-list
      :desc "Tags search"           "m"          #'org-tags-view
      :desc "View search"           "v"          #'org-search-view
      :desc "List Agenda Files"     "F"          #'+jg-org-list-agenda-files
      :desc "occur-in-agenda-files" "/"          #'org-occur-in-agenda-files
      :desc "agenda-file-to-front"  "f"          #'org-agenda-file-to-front
      :desc "remove-file"           "r"          #'org-remove-file
      :desc "agenda-list"           "l"          #'org-agenda-list
      )
;;-- end leader map

;;-- <leaderless>
(map! :map org-mode-map
      ;; :n "I" nil
      ;; :n "] h" nil
      ;; :n "[ h" nil
      ;; :desc "Cycle"           :n "a"   #'org-cycle
      :desc "Next Link"       :n "] l" #'org-next-link
      :desc "Prev Link"       :n "[ l" #'org-previous-link
      :desc "Forward Heading" :n "] j" #'org-forward-heading-same-level
      :desc "Back Heading"    :n "[ j" #'org-backward-heading-same-level
      )
;;-- end <leaderless>

(map! :map org-mode-map
      :desc "Headings Helm"   :n "s h" #'helm-org-in-buffer-headings
      :desc "Hide Drawers"    :n "z d" (cmd! (org-cycle-hide-drawers 'org-cycle-hide-drawers))
      )


;;-- <localleader>
(map! :map org-mode-map
      :localleader
      :desc "Refile"                 "R" #'+jg-org-refile-subtree
      :desc "Todo"                   "TAB" #'org-todo

      (:prefix ("f". "Format")
       :desc "Clean Org"             "c" #'+jg-org-clean-master
       :desc "Wrap Numbers"          "w" #'+jg-org-wrap-numbers
       :desc "Wrap non-link urls"    "L" #'+jg-org-wrap-non-link-urls
       :desc "Remove Duplicates"     "D" #'+jg-org-remove-duplicate-tweet-entries
       )
      ;; TODO refine this Codeblocks
      (:prefix ("." . "Code Blocks")
       :desc "Edit Codeblock "     "e" #'org-edit-src-code
       :desc "Exec Codeblock"      "E" #'org-babel-execute-src-block)
      ;; Links
      (:prefix ("l" . "Links")
       ;; "o"   #'+jg-org-open_link_in_buffer
       ;; "O"   #'+jg-org-open_link_externally
       :desc "Change Link Name"    "n" #'+jg-org-change-link-name
       )
      ;; Insertion
      (:prefix ("i" . "Insert")
       :desc "Insert Heading Trio" "t" #'+jg-org-insert-heading-trio
       :desc "Insert Subheading"   "h" #'org-insert-subheading
       :desc "Insert Drawer"       "d" #'org-insert-drawer)
      )
;;-- end <localleader>

;;-- evil-org
(map! :map evil-org-mode-map
      :n "I"   nil
      :n "za"  nil
      :n "zA"  nil
      :n "zc"  nil
      :n "zC"  nil
      :n "zm"  nil
      :n "zM"  nil
      :n "zo"  nil
      :n "zR"  nil

      :m "] h"  nil
      :m "[ h"  nil
      )
;;-- end evil-org

;;-- dired
(map! :map dired-mode-map
      :after jg-dired-bindings
      (:prefix "%"
       :desc "Mark Orgs" :n "o"     #'+jg-org-dired-select-org
       )
      :localleader
      (:prefix ("m" . "Mark")
       :desc "Mark Orgs" "o" #'+jg-org-dired-select-org
       )
      (:prefix ("f" . "Find")
       :desc "Display Selection" "s" #'+jg-org-display-selection
       )
      (:prefix ("K" . "Destructive")
       :desc "Clean Marked" "c"     #'+jg-org-dired-clean
       :desc "Mark as Twitter"  "T" #'+jg-org-dired-add-twitter-prop
       )
      )
;;-- end dired

(map! :after org-agenda
      :map org-agenda-mode-map
      :localleader
      (:prefix ("d" . "Date/time"))
      (:prefix ("c" . "Clock"))
      (:prefix ("p" . "Priority")))

(map! :after graphviz-dot-mode
      :map graphviz-dot-mode-map
      :localleader
      )
