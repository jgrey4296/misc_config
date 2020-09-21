
;;(after! hydra
;;  (defhydra tag-clean
;;                "
;;                | Commands   ^^|
;;                |------------^^|------------^^|
;;                | [_q_] Quit   | [_!_] Split  |
;;                | [_f_] Filter | [_p_] Prev   |
;;                | [_s_] Sub    | [_l_] Leave  |
;;                "
;;                ("q" nil :exit t)
;;                ("f" #'tag-clean/mark-to-filter)
;;                ("s" #'tag-clean/mark-to-sub)
;;                ("p" #'tag-clean/previous)
;;                ("l" #'tag-clean/leave)
;;                ("!" #'jg-tag-org-split-on-headings :exit t)
;;                )
;;  )
;;
(map! :after tag-clean-minor-mode
      :map 'tag-clean-minor-mode-map
      "." #'tag-clean/body
      )

(map! :after dired
      :mode dired-mode
      :localleader
      (:prefix ("K" . "Destructive")
       :n "c" #'jg-tag-clean-marked-files
       :n "C" #'jg-tag-chop-long-files-from-dired
       :n "B" #'jg-tag-unify-pdf-locations
       :n "Z" #'jg-tag-quick-compress-orgs
       :n "J" #'jg-tag-reformat-jsons
       )
      :n "m u" #'jg-tag-mark-untagged-orgs

      :n "d u" #'jg-tag-dired-directory-count-untagged
      :n "d t" #'jg-tag-describe-marked-tags

      :n "f r" #'jg-tag-find-random-marked-file
      :n "f s" #'jg-tag-display-selection

      :n "i p" #'jg-tag-index-people
      :n "i t" #'jg-tag-index-tags
      )
(map! :after helm
      :leader
      (:prefix ("ah" . "Helms")
       "f" #'jg-tag-helm-bookmarks
       "t" #'jg-tag-helm-twitter
       "h" #'jg-tag-helm-heading-twitter
       "u" #'jg-tag-helm-unified
       "b" #'jg-tag-helm-bibtex
       )
      )
(map! :after org
      :mode org-mode
      :leader
      (:prefix "ao"
       :n "c" #'jg-tag-clean-org
       :n "w" #'jg-tag-wrap-numbers
       :n "L" #'jg-tag-wrap-non-link-urls
       :n "D" #'jg-tag-remove-duplicates
       :n "x s" #'jg-tag-next-similar-string
       )
      )
(map! :leader
      "x l s" #'jg-tag-split-on-char-n
)
