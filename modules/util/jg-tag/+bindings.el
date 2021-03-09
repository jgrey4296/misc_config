
(after! hydra
  (defhydra tag-clean ()
    "
               | Commands   ^^|
               |------------^^|------------^^|
               | [_q_] Quit   | [_!_] Split  |
               | [_f_] Filter | [_p_] Prev   |
               | [_s_] Sub    | [_l_] Leave  |
               "
    ("q" nil :exit t)
    ("f" #'tag-clean/mark-to-filter)
    ("s" #'tag-clean/mark-to-sub)
    ("p" #'tag-clean/previous)
    ("l" #'tag-clean/leave)
    ("!" #'jg-tag-org-split-on-headings :exit t)
    )
  (map! :map tag-clean-minor-mode-map
        "." #'tag-clean/body
        )
  )

;; Helm bindings
(map! :leader
      :prefix ("ah" . "Helms")
      :desc "Bookmark Helm" "f" #'jg-tag-helm-bookmarks
      :desc "Tag Helm" "t" #'jg-tag-helm-twitter
      :desc "Twitter Helm" "h" #'jg-tag-helm-heading-twitter
      :desc "Unified Helm" "u" #'jg-tag-helm-unified
      :desc "Bibtex Helm" "b" #'jg-tag-helm-bibtex
      )
(map! :map helm-map
      "M-SPC" #'helm-next-page)
;; Text bindings
(map! :leader
      :prefix "x"
      "l s" #'jg-tag-split-on-char-n
      "s"   #'jg-tag-next-similar-string
      )
;; Bibtex bindings
(map! :map bibtex-mode-map
      :localleader
      :desc "Bibtex Hydra" "." #'jg-org-ref-bibtex-hydra/body
      :desc "New Entry" "n" #'org-ref-bibtex-new-entry/body
      ;; Citation
      ;; TODO : put this in org-mode bindings? : "i " 'org-reftex-citation
      :desc "Open Pdf" "p" #'+jg-org-ref-open-bibtex-pdf
      ;; TODO add open bibtex dir...
      )

;; Dired bindings
(map! :after dired
      :map (dired-mode-map ranger-mode-map)
      :localleader
      (:prefix ("d" . "Describe")
       :desc "Count Untagged Orgs" "u"   #'jg-tag-dired-directory-count-untagged
       :desc "Describe Marked Tags" "t"  #'jg-tag-describe-marked-tags
       )
      (:prefix ("K" . "Destructive")
       :desc "Clean Marked" "c"          #'jg-tag-clean-marked-files
       :desc "Chop File Names" "C"       #'jg-tag-chop-long-files-from-dired
       :desc "Unify Pdf Locations" "U"   #'jg-tag-unify-pdf-locations
       :desc "Quick Compress" "Z"        #'jg-tag-quick-compress-orgs
       :desc "Reformat Json" "J"         #'jg-tag-reformat-jsons
       )
      (:prefix ("m" . "Mark")
       :desc "Mark Untagged Orgs" "u"    #'jg-tag-mark-untagged-orgs
       )
      (:prefix ("f" . "Find")
       :desc "Find Random Marked" "r"    #'jg-tag-find-random-marked-file
       :desc "Display Tag Selection" "s" #'jg-tag-display-selection
       )
      (:prefix ("i" . "Index")
       :desc "Index People" "p"          #'jg-tag-index-people
       :desc "Index Tags" "t"            #'jg-tag-index-tags
       )
      )
