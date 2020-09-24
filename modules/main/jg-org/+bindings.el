;;; main/jg-org/+bindings.el -*- lexical-binding: t; -*-

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

(map! :after org
      :mode org-mode
        :n "g j" #'org-forward-heading-same-level
        :n "g k" #'org-backward-heading-same-level
        :n "] p" #'org-next-link
        :n "[ p" #'org-previous-link
        :n  "g h" #'helm-org-in-buffer-headings
        "." nil
        :localleader
        ;; SRC CODE
        ". e"   'org-edit-src-code
        ". E"   'org-babel-execute-src-block
        ;; Links
        ". d"   'org-toggle-link-display
        ". o"   '+jg-org-open_link_in_buffer
        ". O"   '+jg-org-open_link_externally
        ". n"   '+jg-org-change_link_name
        ;;Formatting
        "i t"   '+jg-org-insert-heading-trio

        )
