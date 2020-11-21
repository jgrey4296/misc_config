
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
(map! :map tag-clean-minor-mode-map
      "." #'tag-clean/body
      )
(map! :leader
      (:prefix ("ah" . "Helms")
       "f" #'jg-tag-helm-bookmarks
       "t" #'jg-tag-helm-twitter
       "h" #'jg-tag-helm-heading-twitter
       "u" #'jg-tag-helm-unified
       "b" #'jg-tag-helm-bibtex
       )
      )

;; (map! :leader
;;       (:prefix ("ao" . "Org")
;;        :n "c" #'jg-tag-clean-org
;;        :n "w" #'jg-tag-wrap-numbers
;;        :n "L" #'jg-tag-wrap-non-link-urls
;;        :n "D" #'jg-tag-remove-duplicates
;;        (:prefix "x"
;;        :n "s" #'jg-tag-next-similar-string)
;;        )
;;       )

(progn
  (progn
    (doom--define-leader-key
     :states 'normal
     :infix "ao"
     "c" #'jg-tag-clean-org
     "w" #'jg-tag-wrap-numbers
     "L" #'jg-tag-wrap-non-link-urls
     "D" #'jg-tag-remove-duplicates)
    (doom--define-leader-key
     :infix "ao" "" (list
                     :ignore t
                     :which-key "Org")))
  (doom--define-leader-key
   :states 'normal
   :infix (general--concat nil "ao" "x")
   "s" #'jg-tag-next-similar-string))





(map! :leader
      "x l s" #'jg-tag-split-on-char-n
)

;; (map! :map bibtex-mode-map
;;       :localleader
;;       :n "." #'jg-org-ref-bibtex-hydra/body
;;       :n "n" #'org-ref-bibtex-new-entry/body
;;       ;; Citation
;;       ;; TODO : put this in org-mode bindings? : "i " 'org-reftex-citation
;;       :n "p" #'+jg-org-ref-open-bibtex-pdf
;;       ;; TODO add open bibtex dir...
;; )

;; (define-localleader-key! :states 'normal :keymaps
;;   '(bibtex-mode-map)
;;   "." #'jg-org-ref-bibtex-hydra/body
;;   "n" #'org-ref-bibtex-new-entry/body
;;   "p" #'+jg-org-ref-open-bibtex-pdf)

(general-define-key :states '(normal visual motion)
                    :major-modes t
                    :prefix doom-localleader-key
                    :states 'normal
                    :keymaps '(bibtex-mode-map)
                    "." #'jg-org-ref-bibtex-hydra/body
                    "n" #'org-ref-bibtex-new-entry/body
                    "p" #'+jg-org-ref-open-bibtex-pdf
                    "s" #'+jg-org-ref-search-scholar
)

(map! :map helm-map
      "M-SPC" #'helm-next-page)
