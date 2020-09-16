(setq-default jg-tag-unify-layer/loc-bookmarks "~/github/writing/resources/main_bookmarks.html"
              jg-tag-unify-layer/loc-bibtex "~/github/writing/resources/years"
              jg-tag-unify-layer/twitter-account-index "~/.spacemacs.d/setup_files/tw_acct.index"
              jg-tag-unify-layer/twitter-tag-index "~/.spacemacs.d/setup_files/tw_tag.index"
              jg-tag-unify-layer/global-tags-location "~/github/writing/resources/collate.tags"

              jg-tag-unify-layer/twitter-helm-candidates nil
              jg-tag-unify-layer/twitter-heading-helm-candidates nil
              jg-tag-unify-layer/preferred-linecount-for-org 1500
              jg-tag-unify-layer/loc-master-tag-list ""
              jg-tag-unify-layer/org-clean-marker nil

              bibtex-completion-bibliography nil
              bibtex-completion-additional-search-fields '("tags" "year")
              bibtex-completion-pdf-field "file"
              bibtex-completion-pdf-open-function (lambda (x) (org-open-link-from-string (format "[[file:%s]]" x)))

              jg-tag-unify-layer/helm-bibtex-candidates nil

              jg-tag-unify-layer/all-author-list '()
              jg-tag-unify-layer/all-tag-list '()
              jg-tag-unify-layer/global-tags (make-hash-table :test 'equal)
              jg-tag-unify-layer/jg-tag-unify-layer-candidates-names '()
              jg-tag-unify-layer/jg-tag-unify-layer-candidate-counts '()
              ;; Start Position -> End Line number because of changes in positions from tag add/retract
              jg-tag-unify-layer/jg-tag-unify-layer-marker (make-marker)
              jg-tag-unify-layer/last-similarity-arg 1

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

(add-hook 'bibtex-mode-hook
          (lambda ()
            (let ((misc (assoc "Misc" bibtex-BibTeX-entry-alist))
                  (copied (assoc-delete-all "Misc" (copy-alist bibtex-BibTeX-entry-alist)))
                  (custom '("Misc" "Miscellaneous" nil nil (("author") ("title" "Title of the work (BibTeX converts it to lowercase)") ("howpublished" "The way in which the work was published") ("month") ("year") ("file")))))
              (setq bibtex-BibTeX-entry-alist (cons custom copied))
              )
            ))

(after! helm
  (setq helm-grep-actions (append helm-grep-actions '(("Open Url" . jg-tag-unify-layer/open-url-action))))
  ;; Build a Custom grep for bookmarks
  (setq jg-tag-unify-layer/bookmark-helm-source
        (helm-make-source "Bookmark Helm" 'helm-grep-class
          :action (helm-make-actions "Open Url" 'jg-tag-unify-layer/open-url-action
                                     "Insert"   'jg-tag-unify-layer/insert-candidates
                                     "Insert Link" 'jg-tag-unify-layer/insert-links
                                     "Tweet Link"  'jg-tag-unify-layer/tweet-link-action
                                     )
          :filter-one-by-one 'jg-tag-unify-layer/grep-filter-one-by-one
          :nomark nil
          :backend helm-grep-default-command
          :pcre nil
          )
        jg-tag-unify-layer/twitter-helm-source
        (helm-make-source "Twitter Helm" 'helm-source
          :action (helm-make-actions "File Select Helm" 'jg-tag-unify-layer/file-select-helm)
          )
        jg-tag-unify-lauyer/twitter-heading-helm-source
        (helm-make-source "Twitter Heading Helm" 'helm-source
          :action (helm-make-actions "File Select Helm" 'jg-tag-unify-layer/file-select-helm)
          )
        jg-tag-unify-layer/file-select-source
        (helm-make-source "Twitter File Select Helm" 'helm-source
          :action (helm-make-actions "Find File" 'jg-tag-unify-layer/find-file)
          )

        jg-tag-unify-layer/jg-tag-unify-layer-helm
        (helm-make-source "Helm Tagging" 'helm-source
          :action (helm-make-actions "Set" 'jg-tag-unify-layer/set-tags))

        jg-tag-unify-layer/jg-tag-unify-layer-fallback-source
        (helm-make-source "Helm Fallback Source" 'helm-source
          :action (helm-make-actions "Create" 'jg-tag-unify-layer/set-new-tag)
          :filtered-candidate-transformer (lambda (_c _s) (list helm-pattern)))
          )
  (spacemacs/declare-prefix "a h" "Helms")
  (spacemacs/set-leader-keys
   "a h f" 'jg-tag-unify-layer/helm-bookmarks
   "a h t" 'jg-tag-unify-layer/helm-twitter
   "a h h" 'jg-tag-unify-layer/helm-heading-twitter
   "a h u" 'jg-tag-unify-layer/helm-unified
   )

  (defun jg-tag-unify-layer/file-select-helm (candidates)
    " Given a list of Files, provide a helm to open them "
    (interactive)
    ;; (message "File Select Helm Candidates: %s" (helm-marked-candidates))
    ;;process candidates?
    (let*((all-candidates (if (helm-marked-candidates) (-flatten (helm-marked-candidates)) candidates))
          (source (cons `(candidates . ,all-candidates) jg-tag-unify-layer/file-select-source)))
      (helm :sources source
            :full-frame t
            :buffer "*helm file select*"
            )
      )
    )
  (defun jg-tag-unify-layer/helm-twitter ()
    "Run a Helm for searching twitter users"
    (interactive)
    ;;if twitter info not loaded, load
    (if (null jg-tag-unify-layer/twitter-helm-candidates)
        (with-temp-buffer
          (setq jg-tag-unify-layer/twitter-helm-candidates '())
          (insert-file jg-tag-unify-layer/twitter-account-index)
          (goto-char (point-min))
          (let (curr)
            (while (< (point) (point-max))
              (setq curr (split-string (buffer-substring (point) (line-end-position)) ":"))
              (push `(,(car curr) . ,(cdr curr)) jg-tag-unify-layer/twitter-helm-candidates)
              (forward-line)
              )
            )
          )
      )
    ;;add candidates to source
    (let ((source (cons `(candidates . jg-tag-unify-layer/twitter-helm-candidates) jg-tag-unify-layer/twitter-helm-source)))
      ;;call helm
      (helm :sources source
            :full-frame t
            :buffer "*helm twitter*"
            :truncate-lines t
            ;;TODO: is this necessary?
            :candidates jg-tag-unify-layer/twitter-helm-candidates
            )
      )
    )
  (defun jg-tag-unify-layer/helm-heading-twitter ()
    "Run a Helm for searching twitter users"
    (interactive)
    ;;if twitter info not loaded, load
    (if (null jg-tag-unify-layer/twitter-heading-helm-candidates)
        (with-temp-buffer
          (setq jg-tag-unify-layer/twitter-heading-helm-candidates '())
          (insert-file jg-tag-unify-layer/twitter-tag-index)
          (goto-char (point-min))
          (let (curr)
            (while (< (point) (point-max))
              (setq curr (split-string (buffer-substring (point) (line-end-position)) ":"))
              (push `(,(car curr) . ,(cdr curr)) jg-tag-unify-layer/twitter-heading-helm-candidates)
              (forward-line)
              )
            )
          )
      )
    ;;add candidates to source
    (let ((source (cons `(candidates . jg-tag-unify-layer/twitter-heading-helm-candidates) jg-tag-unify-layer/twitter-helm-source)))
      ;;call helm
      (helm :sources source
            :full-frame t
            :buffer "*helm twitter heading*"
            :truncate-lines t
            ;;TODO: is this necessary?
            :candidates jg-tag-unify-layer/twitter-heading-helm-candidates
            )
      )
    )
  (defun jg-tag-unify-layer/helm-bookmarks ()
    " Run a Helm for search and opening html bookmarks "
    (interactive)
    (helm-set-local-variable
     'helm-grep-include-files (format "--include=%s" jg-tag-unify-layer/loc-bookmarks)
     'helm-grep-last-targets `(,jg-tag-unify-layer/loc-bookmarks)
     'default-directory "~/github/writing/resources/"
     )
    (helm :sources jg-tag-unify-layer/bookmark-helm-source
          :full-frame t
          :buffer "*helm bookmarks*"
          :truncate-lines t
          )
    )

  (defun jg-tag-unify-layer/helm-unified (arg)
    (interactive "P")
    ;;Clear Cache if necessary
    (when arg
      (bibtex-completion-clear-cache))
    ;;Load headings if necessary
    (if (null jg-tag-unify-layer/twitter-heading-helm-candidates)
        (with-temp-buffer
          (setq jg-tag-unify-layer/twitter-heading-helm-candidates '())
          (insert-file jg-tag-unify-layer/twitter-tag-index)
          (goto-char (point-min))
          (let (curr)
            (while (< (point) (point-max))
              (setq curr (split-string (buffer-substring (point) (line-end-position)) ":"))
              (push `(,(car curr) . ,(cdr curr)) jg-tag-unify-layer/twitter-heading-helm-candidates)
              (forward-line)
              )
            )
          )
      )
    ;;Load twitter users if necessary
    (if (null jg-tag-unify-layer/twitter-helm-candidates)
        (with-temp-buffer
          (setq jg-tag-unify-layer/twitter-helm-candidates '())
          (insert-file jg-tag-unify-layer/twitter-account-index)
          (goto-char (point-min))
          (let (curr)
            (while (< (point) (point-max))
              (setq curr (split-string (buffer-substring (point) (line-end-position)) ":"))
              (push `(,(car curr) . ,(cdr curr)) jg-tag-unify-layer/twitter-helm-candidates)
              (forward-line)
              )
            )
          )
      )
    ;;Set local variables for bookmarks
    (helm-set-local-variable
     'helm-grep-include-files (format "--include=%s" jg-tag-unify-layer/loc-bookmarks)
     'helm-grep-last-targets `(,jg-tag-unify-layer/loc-bookmarks)
     'default-directory "~/github/writing/resources/"
     )

    ;;add candidates to source
    (let* ((bibtex-completion-additional-search-fields '("tags" "year"))
           (candidates-bibtex (if (or arg (null jg-tag-unify-layer/helm-bibtex-candidates))
                                  (progn (message "Generating Candidates")
                                         (bibtex-completion-init)
                                         (setq jg-tag-unify-layer/helm-bibtex-candidates
                                               (mapcar 'jg-tag-unify-layer/process-candidates (bibtex-completion-candidates)))
                                         jg-tag-unify-layer/helm-bibtex-candidates)
                                jg-tag-unify-layer/helm-bibtex-candidates
                                ))
           (source-tw (cons `(candidates . jg-tag-unify-layer/twitter-helm-candidates) jg-tag-unify-layer/twitter-helm-source))
           (source-heading (cons `(candidates . jg-tag-unify-layer/twitter-heading-helm-candidates) jg-tag-unify-layer/twitter-helm-source)))
      ;;call helm
      (helm :sources '(source-heading jg-tag-unify-layer/helm-source-bibtex jg-tag-unify-layer/bookmark-helm-source )
            :full-frame t
            :buffer "*Helm unified*"
            :truncate-lines t
            :bibtex-candidates candidates-bibtex
            )
      )
    )
)
(after! helm-bibtex
  ;; Keybind my bib helm
  ;; TODO (spacemacs/set-leader-keys "a h b" 'jg-tag-unify-layer/helm-bibtex)

  ;; Define the bib helm
  (defvar jg-tag-unify-layer/helm-source-bibtex
    '((name . "BibTeX entries")
      (header-name . "BibTeX entries")
      ;; (lambda (name) (format "%s%s: " name (if helm-bibtex-local-bib " (local)" ""))))
      (candidates . helm-bibtex-candidates)
      (match helm-mm-exact-match helm-mm-match helm-fuzzy-match)
      (fuzzy-match)
      (redisplay . identity)
      (multimatch)
      (group . helm)
      (filtered-candidate-transformer helm-bibtex-candidates-formatter helm-flx-fuzzy-matching-sort helm-fuzzy-highlight-matches)
      (action . (("Insert citation"     . helm-bibtex-insert-citation)
                 ("Open PDF"            . helm-bibtex-open-pdf)
                 ("Insert BibTeX key"   . helm-bibtex-insert-key)
                 ("Insert BibTeX entry" . helm-bibtex-insert-bibtex)
                 ("Show entry"          . helm-bibtex-show-entry)
                 )))
    "Simplified source for searching bibtex files")
  (defun jg-tag-unify-layer/helm-bibtex (&optional arg local-bib input)
    " Custom implementation of helm-bibtex"
    (interactive "P")
    (require 'helm-bibtex)
    (when arg
      (bibtex-completion-clear-cache))
    (let* ((bibtex-completion-additional-search-fields '("tags" "year"))
           (candidates (if (or arg (null jg-tag-unify-layer/helm-bibtex-candidates))
                           (progn (message "Generating Candidates")
                                  (bibtex-completion-init)
                                  (setq jg-tag-unify-layer/helm-bibtex-candidates
                                        (mapcar 'jg-tag-unify-layer/process-candidates (bibtex-completion-candidates)))
                                  jg-tag-unify-layer/helm-bibtex-candidates)
                         jg-tag-unify-layer/helm-bibtex-candidates
                         ))
           )
      (helm :sources `(,jg-tag-unify-layer/helm-source-bibtex)
            :full-frame helm-bibtex-full-frame
            :buffer "*helm bibtex*"
            :input input
            :bibtex-local-bib local-bib
            :bibtex-candidates candidates
            )))
  (eval-after-load 'f (progn (jg-tag-unify-layer/build-bibtex-list) nil))
)
(after! org
  (defun jg-tag-unify-layer/org-mod-map ()
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      ". c" 'jg-tag-unify-layer/clean-org
      ". w" 'jg-tag-unify-layer/wrap-numbers
      ". L" 'jg-tag-unify-layer/wrap-non-link-urls
      ". D" 'jg-tag-unify-layer/remove-duplicates
      "x s" 'jg-tag-unify-layer/next-similar-string
      )
    )
  (add-hook 'org-mode-hook 'jg-tag-unify-layer/org-mod-map)

  (jg-tag-unify-layer/rebuild-tag-database)

  (evil-define-operator jg-tag-unify-layer/jg-tag-unify-layer-helm-start (beg end)
    """ Opens the Tagging Helm """
    (interactive "<R>")
    (set-marker jg-tag-unify-layer/jg-tag-unify-layer-marker (if (eq evil-state 'visual)  evil-visual-end (line-end-position)))
    (let* ((candidates (jg-tag-unify-layer/jg-tag-unify-layer-candidates))
           (main-source (cons `(candidates . ,(mapcar 'car candidates)) jg-tag-unify-layer/jg-tag-unify-layer-helm))
           )
      (helm :sources '(main-source jg-tag-unify-layer/jg-tag-unify-layer-fallback-source)
            :input "")
      ))
  )
(after! dired
  ;; TODO (spacemacs/declare-prefix-for-mode 'dired-mode "m i" "Index")

  (map! :mode dired-mode
        :localleader
    "K c" 'jg-tag-unify-layer/clean-marked-files
    "K C" 'jg-tag-unify-layer/chop-long-files-from-dired
    "K B" 'jg-tag-unify-layer/unify-pdf-locations
    "K Z" 'jg-tag-unify-layer/quick-compress-orgs
    "K J" 'jg-tag-unify-layer/reformat-jsons

    "m u" 'jg-tag-unify-layer/mark-untagged-orgs

    "d u" 'jg-tag-unify-layer/dired-directory-count-untagged
    "d t" 'jg-tag-unify-layer/describe-marked-tags

    "f r" 'jg-tag-unify-layer/find-random-marked-file
    "f s" 'jg-tag-unify-layer/display-selection

    "i p" 'jg-tag-unify-layer/index-people
    "i t" 'jg-tag-unify-layer/index-tags
    )
  )
(after! evil
  (evil-ex-define-cmd "t[ag]" 'jg-tag-unify-layer/jg-tag-unify-layer-helm-start)
  (evil-ex-define-cmd "to" 'jg-tag-unify-layer/tag-occurrences)
  (evil-ex-define-cmd "toa" 'jg-tag-unify-layer/tag-occurrences-in-open-buffers)
  (evil-ex-define-cmd "tv"  'org-tags-view)
  (evil-ex-define-cmd "ts"  'org-set-tags)
  )
(after! tag-clean-minor-mode ()
  (push 'tag-clean-minor-mode minor-mode-list)
  ;; TODO
  ;; (spacemacs|define-transient-state tag-clean
  ;;                                   :title "Tag Cleaning Transient State"
  ;;               :doc (concat "
  ;;               | Commands   ^^|
  ;;               |------------^^|------------^^|
  ;;               | [_q_] Quit   | [_!_] Split  |
  ;;               | [_f_] Filter | [_p_] Prev   |
  ;;               | [_s_] Sub    | [_l_] Leave  |
  ;;               ")
  ;;               :bindings
  ;;               ("q" nil :exit t)
  ;;               ("f" tag-clean/mark-to-filter)
  ;;               ("s" tag-clean/mark-to-sub)
  ;;               ("p" tag-clean/previous)
  ;;               ("l" tag-clean/leave)
  ;;               ("!" jg-tag-unify-layer/org-split-on-headings :exit t)
  ;;               )
  ;;             (spacemacs/set-leader-keys-for-minor-mode 'tag-clean-minor-mode
  ;;               "." 'spacemacs/tag-clean-transient-state/body
  ;;               )
  ;;            )
    )
