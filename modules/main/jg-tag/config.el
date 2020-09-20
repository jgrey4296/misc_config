(setq-default jg-tag-loc-bookmarks "~/github/writing/resources/main_bookmarks.html"
              jg-tag-loc-bibtex "~/github/writing/resources/years"
              jg-tag-twitter-account-index "~/.doom.d/setup_files/tw_acct.index"
              jg-tag-twitter-tag-index "~/.doom.d/setup_files/tw_tag.index"
              jg-tag-global-tags-location "~/github/writing/resources/collate.tags"

              jg-tag-twitter-helm-candidates nil
              jg-tag-twitter-heading-helm-candidates nil
              jg-tag-preferred-linecount-for-org 1500
              jg-tag-loc-master-tag-list ""
              jg-tag-org-clean-marker nil

              bibtex-completion-bibliography nil
              bibtex-completion-additional-search-fields '("tags" "year")
              bibtex-completion-pdf-field "file"
              bibtex-completion-pdf-open-function #'(lambda (x) (org-open-link-from-string (format "[[file:%s]]" x)))

              jg-tag-helm-bibtex-candidates nil

              jg-tag-all-author-list '()
              jg-tag-all-tag-list '()
              jg-tag-global-tags (make-hash-table :test 'equal)
              jg-tag-candidates-names '()
              jg-tag-candidate-counts '()
              ;; Start Position -> End Line number because of changes in positions from tag add/retract
              jg-tag-marker (make-marker)
              jg-tag-last-similarity-arg 1

              ;; Bibtex optional fields
              bibtex-user-optional-fields '(("annotation" "Personal Annotation")
                                            ("tags" "Set of tags")
                                            ("isbn" "ISBN of file")
                                            ("doi" "DOI of file")
                                            ("url" "Url of file")
                                            ("file" "The path of the file")
                                            ("translator" "The Translators of the work")
                                            )

              )

(after! (helm evil)
  (evil-ex-define-cmd "t[ag]" 'jg-tag-helm-start)
  (evil-ex-define-cmd "to" 'jg-tag-occurrences)
  (evil-ex-define-cmd "toa" 'jg-tag-occurrences-in-open-buffers)
  (evil-ex-define-cmd "tv"  'org-tags-view)
  (evil-ex-define-cmd "ts"  'org-set-tags)
  )
(after! tag-clean-minor-mode ()
  (push 'tag-clean-minor-mode minor-mode-list)
  ;; TODO
  ;; (defhydra tag-clean
  ;;               "
  ;;               | Commands   ^^|
  ;;               |------------^^|------------^^|
  ;;               | [_q_] Quit   | [_!_] Split  |
  ;;               | [_f_] Filter | [_p_] Prev   |
  ;;               | [_s_] Sub    | [_l_] Leave  |
  ;;               "
  ;;               ("q" nil :exit t)
  ;;               ("f" tag-clean/mark-to-filter)
  ;;               ("s" tag-clean/mark-to-sub)
  ;;               ("p" tag-clean/previous)
  ;;               ("l" tag-clean/leave)
  ;;               ("!" jg-tag-org-split-on-headings :exit t)
  ;;               )
  ;;             (:map! :map 'tag-clean-minor-mode-map
  ;;               "." 'tag-clean/body
  ;;               )
  ;;            )
    )
