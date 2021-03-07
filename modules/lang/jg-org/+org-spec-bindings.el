;;; lang/jg-org/+org-spec-bindings.el -*- lexical-binding: t; -*-
;;; Paren Control
(map! :map org-mode-map
      "C-c [" nil
      "C-c ]" nil
      )

;;; Personal
(map! :map org-mode-map
      (:leader
       :prefix "t"
       "l"   'org-toggle-link-display)
      (:prefix "g"
       :n "j" #'org-forward-heading-same-level
       :n "k" #'org-backward-heading-same-level
       :n "h" #'helm-org-in-buffer-headings)
      (:prefix "]"
       :n "p" #'org-next-link)
      (:prefix "["
       :n "p" #'org-previous-link)
      )



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
