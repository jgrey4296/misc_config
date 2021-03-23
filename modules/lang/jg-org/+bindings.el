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
