;;; main/jg-org/+bindings.el -*- lexical-binding: t; -*-


;;; Academic phrases
(map! :leader
      (:prefix ("xA" . "Academic Phrases")
       "p" #'academic-phrases
       "s" #'academic-phrases-by-section)
      ;; AGENDA
      (:prefix ("ao" . "Org")
       (:prefix ("a" . "Agenda")
        "/"   'org-occur-in-agenda-files
        "f"   'org-agenda-file-to-front
        "r"   'org-remove-file
        "l"   'org-agenda-list
        "F"   '+jg-org-list-agenda-files
        "t"   'org-tags-view)
       )
      (:prefix "i"
       "t" #'org-time-stamp)
      (:prefix "j"
       "c" #'org-goto-calendar)
      "a U r" #'+jg-org-bibtex-load-random)

;;; Paren Control
(map! :after org
      :map org-mode-map
      "C-c [" nil
      "C-c ]" nil
      )

;;; Personal
(map! :after org
      :map org-mode-map
      (:leader
       :prefix "t"
       "l"   'org-toggle-link-display)
      (:prefix "g"
        "j" #'org-forward-heading-same-level
        "k" #'org-backward-heading-same-level
        "h" nil
        "h" #'helm-org-in-buffer-headings)
      (:prefix "]"
        "p" #'org-next-link)
      (:prefix "["
        "p" #'org-previous-link)
      (:localleader
        "." nil
       ;; SRC CODE
       (:prefix ("." . "JG Custom")
        :desc "Edit Codeblock " "e"   'org-edit-src-code
         :desc "Exec Codeblock" "E"   'org-babel-execute-src-block
        ;; Links
        (:prefix ("l" . "Links")
         ;; "o"   '+jg-org-open_link_in_buffer
         ;; "O"   '+jg-org-open_link_externally
         "n"   '+jg-org-change_link_name
         )
        )
       "i" nil
       ;;Formatting
       (:prefix ("i" . "Insert")
        "t"   '+jg-org-insert-heading-trio
        "h"   'org-insert-subheading
        "d" #'org-insert-drawer
        )
       )
      )
