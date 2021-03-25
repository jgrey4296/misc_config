
;; Helm bindings
(map! :leader
      :prefix ("a h" . "Helms")
      :desc "Firefox Helm"              "f" #'+jg-tag-helm-bookmarks
      :desc "Tag Helm"                  "t" #'+jg-tag-helm-twitter
      :desc "Twitter Helm"              "h" #'+jg-tag-helm-heading-twitter
      :desc "Unified Helm"              "u" #'+jg-tag-helm-unified
      )
(map! :map helm-map
      "M-SPC" #'helm-next-page)

;; Dired bindings
(map! :after dired
      :map (dired-mode-map ranger-mode-map)
      :localleader
      (:prefix ("d" . "Describe")
       :desc "Count Untagged Orgs" "u"   #'+jg-tag-dired-directory-count-untagged
       :desc "Describe Marked Tags" "t"  #'+jg-tag-describe-marked-tags
       )
      (:prefix ("m" . "Mark")
       :desc "Mark Untagged Orgs" "u"    #'+jg-tag-mark-untagged-orgs
       )
      (:prefix ("f" . "Find")
       :desc "Display Tag Selection" "s" #'+jg-tag-display-selection
       )
      (:prefix ("i" . "Index")
       :desc "Index People" "p"          #'+jg-tag-index-people
       :desc "Index Tags" "t"            #'+jg-tag-index-tags
       )
      (:prefix ("t" . "remove Tag")
       :desc "Ivy Store Tag" #'+jg-tag-ivy-tag-set
       :desc "Ivy Clear Tag" #'+jg-tag-ivy-tag-clear
       )
      )

;; Evil ex commands
(after! (helm evil)
  (evil-ex-define-cmd "t[ag]"  #'+jg-tag-helm-start)
  (evil-ex-define-cmd "to"     #'+jg-tag-occurrences)
  (evil-ex-define-cmd "toa"    #'+jg-tag-occurrences-in-open-buffers)
  (evil-ex-define-cmd "tv"     #'org-tags-view)
  (evil-ex-define-cmd "ts"     #'org-set-tags)

  )
;; Tag Cleaning hydra
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
    ("!" #'+jg-tag-org-split-on-headings :exit t)
    )
  (map! :map tag-clean-minor-mode-map
        "." #'tag-clean/body
        )
  )
