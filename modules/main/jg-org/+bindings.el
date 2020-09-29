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
;; (map! :after org
;;       :map org-agenda-mode-map
;; 	:prefix "v"
;;         "w"   #'org-agenda-week-view
;;         "m"   #'org-agenda-month-view
;;         )

;;; Personal
(map! :map org-mode-map
      (:prefix "g"
       :n "j" #'org-forward-heading-same-level
       :n "k" #'org-backward-heading-same-level)
      (:prefix "]"
       :n "p" #'org-next-link)
      (:prefix "["
       :n "[ p" #'org-previous-link)
      (:prefix "g"
       :n  "h" #'helm-org-in-buffer-headings)
      "." nil
      (:localleader
       :n "." nil
       :n "i" nil
       ;; SRC CODE
       (:prefix "."
        :n "e"   'org-edit-src-code
        :n "E"   'org-babel-execute-src-block
        ;; Links
        :n "d"   'org-toggle-link-display
        :n "o"   '+jg-org-open_link_in_buffer
        :n "O"   '+jg-org-open_link_externally
        :n "n"   '+jg-org-change_link_name
        )
       ;;Formatting
       :prefix "i"
       :n "t"   '+jg-org-insert-heading-trio
       )
      )

