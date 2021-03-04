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
      (:prefix "g"
        "j" #'org-forward-heading-same-level
        "k" #'org-backward-heading-same-level)

      (:prefix "]"
        "p" #'org-next-link)
      (:prefix "["
        "p" #'org-previous-link)
      (:prefix "g"
         "h" #'helm-org-in-buffer-headings)
      "." nil
      (:localleader
        "." nil
        "i" nil
       ;; SRC CODE
       (:prefix ("." . "JG Custom")
         "e"   'org-edit-src-code
         "E"   'org-babel-execute-src-block
        ;; Links
         "d"   'org-toggle-link-display
         "o"   '+jg-org-open_link_in_buffer
         "O"   '+jg-org-open_link_externally
         "n"   '+jg-org-change_link_name
        )
       ;;Formatting
       :prefix ("i" . "Insert")
        "t"   '+jg-org-insert-heading-trio
        "h"   'org-insert-subheading
        "d" #'org-insert-drawer
        )
      )

