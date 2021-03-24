;;; main/jg-org/+bindings.el -*- lexical-binding: t; -*-


;;; Academic phrases
(map! :leader
      (:prefix ("xA" . "Academic Phrases")
       "p"    #'academic-phrases
       "s"    #'academic-phrases-by-section)
      ;; AGENDA
      (:prefix ("ao" . "Org")
       (:prefix ("a" . "Agenda")
        "/"   #'org-occur-in-agenda-files
        "f"   #'org-agenda-file-to-front
        "r"   #'org-remove-file
        "l"   #'org-agenda-list
        "F"   #'+jg-org-list-agenda-files
        "t"   #'org-tags-view)
       )
      (:prefix "i"
       "t"    #'org-time-stamp)
      (:prefix "j"
       "c"    #'org-goto-calendar)
      )
;;; Paren Control
(map! :map org-mode-map
      "C-c [" nil
      "C-c ]" nil
      )
;;; Personal
(map! :map org-mode-map
      (:leader
       :prefix "t"
       :desc "Toggle Links" "l"   'org-toggle-link-display)
      (:prefix "g"
       :desc "Forward Heading" :n "j" #'org-forward-heading-same-level
       :desc "Back Heading" :n "k" #'org-backward-heading-same-level
       :desc "Headings Helm" :n "h" #'helm-org-in-buffer-headings)
      (:prefix "]"
       :desc "Next Link" :n "p" #'org-next-link)
      (:prefix "["
       :desc "Prev Link" :n "p" #'org-previous-link)
      (:localleader
       :prefix ("f". "Format")
       :desc "Fix Drawers" :n "d" #'+jg-org-fix-properties-drawers
       )
      )
;; Misc
(map! :map org-mode-map
      :localleader
      ;; SRC CODE
      (:prefix "."
       :desc "Edit Codeblock " "e"   #'org-edit-src-code
       :desc "Exec Codeblock"  "E"   #'org-babel-execute-src-block
       ;; Links
       (:prefix ("l" . "Links")
        ;; "o"   #'+jg-org-open_link_in_buffer
        ;; "O"   #'+jg-org-open_link_externally
        "n"      #'+jg-org-change_link_name
        )
       )
      "i" nil
      ;;Formatting
      (:prefix ("i" . "Insert")
       "t"   #'+jg-org-insert-heading-trio
       "h"   #'org-insert-subheading
       "d"   #'org-insert-drawer
       )
      )
