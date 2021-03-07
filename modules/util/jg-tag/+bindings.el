
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

(map! :leader
      :prefix ("ah" . "Helms")
      :desc "Bookmark Helm" "f" #'jg-tag-helm-bookmarks
      :desc "Tag Helm" "t" #'jg-tag-helm-twitter
      :desc "Twitter Helm" "h" #'jg-tag-helm-heading-twitter
      :desc "Unified Helm" "u" #'jg-tag-helm-unified
      :desc "Bibtex Helm" "b" #'jg-tag-helm-bibtex
      )


(map! :leader
      :prefix "x"
      "l s" #'jg-tag-split-on-char-n
      "s"   #'jg-tag-next-similar-string
      )

(map! :map bibtex-mode-map
      :localleader
      :desc "Bibtex Hydra" "." #'jg-org-ref-bibtex-hydra/body
      :desc "New Entry" "n" #'org-ref-bibtex-new-entry/body
      ;; Citation
      ;; TODO : put this in org-mode bindings? : "i " 'org-reftex-citation
      :desc "Open Pdf" "p" #'+jg-org-ref-open-bibtex-pdf
      ;; TODO add open bibtex dir...
      )


(map! :map helm-map
      "M-SPC" #'helm-next-page)
