(defconst tag-unify-packages '(
                               helm
                               helm-bibtex
                               dash
                               f
                               org
                               dired
                               evil
                               (tag-clean-minor-mode :location local)
                               )
  )

(defun tag-unify/init-f ()
  (use-package f :defer t)
  )
(defun tag-unify/init-dash ()
  (use-package dash :defer t)
  )
(defun tag-unify/pre-init-helm ()
  (spacemacs|use-package-add-hook helm
    :post-config
    (setq helm-grep-actions (append helm-grep-actions '(("Open Url" . tag-unify/open-url-action))))
    ;; Build a Custom grep for bookmarks
    (setq tag-unify/bookmark-helm-source
          (helm-make-source "Bookmark Helm" 'helm-grep-class
            :action (helm-make-actions "Open Url" 'tag-unify/open-url-action
                                       "Insert"   'tag-unify/insert-candidates
                                       "Insert Link" 'tag-unify/insert-links
                                       "Tweet Link"  'tag-unify/tweet-link-action
                                       )
            :filter-one-by-one 'tag-unify/grep-filter-one-by-one
            :nomark nil
            :backend helm-grep-default-command
            :pcre nil
            )
          )
    )

  (spacemacs/set-leader-keys
    "a B" 'tag-unify/helm-bookmarks)

  (defun tag-unify/helm-bookmarks ()
    (interactive)
    (helm-set-local-variable
     'helm-grep-include-files (format "--include=%s" tag-unify/loc-bookmarks)
     'helm-grep-last-targets `(,tag-unify/loc-bookmarks)
     'default-directory "~/github/writing/resources/"
     )
    (helm :sources tag-unify/bookmark-helm-source
          :full-frame t
          :buffer "*helm bookmarks*"
          :truncate-lines t
          )
    )
  )
(defun tag-unify/pre-init-helm-bibtex ()
  ;; load the bibliography directory on startup
  (setq bibtex-completion-bibliography (tag-unify/build-bibtex-list))
  ;; Keybind my bib helm
  (spacemacs/set-leader-keys "a b" 'tag-unify/helm-bibtex)

  ;; Define the bib helm
  (defvar tag-unify/helm-source-bibtex
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
  (defun tag-unify/helm-bibtex (&optional arg local-bib input)
    " Custom implementation of helm-bibtex"
    (interactive "P")
    (require 'helm-bibtex)
    (when arg
      (bibtex-completion-clear-cache))
    (let* ((bibtex-completion-additional-search-fields '("tags" "year"))
           (candidates (if (or arg (null tag-unify/helm-bibtex-candidates))
                           (progn (message "Generating Candidates")
                                  (bibtex-completion-init)
                                  (setq tag-unify/helm-bibtex-candidates
                                        (mapcar 'tag-unify/process-candidates (bibtex-completion-candidates)))
                                  tag-unify/helm-bibtex-candidates)
                         tag-unify/helm-bibtex-candidates
                         ))
           )
      (helm :sources `(,tag-unify/helm-source-bibtex)
            :full-frame helm-bibtex-full-frame
            :buffer "*helm bibtex*"
            :input input
            :bibtex-local-bib local-bib
            :bibtex-candidates candidates
            )))

  )
(defun tag-unify/post-init-org ()
  (defun tag-unify/org-mod-map ()
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      ". c" 'tag-unify/clean-org
      ". w" 'tag-unify/wrap-numbers
      ". L" 'tag-unify/wrap-non-link-urls
      )
    (evil-define-key 'normal 'evil-org-mode-map
      (kbd "g >") 'org-next-link
      )
    )
  (add-hook 'org-mode-hook 'tag-unify/org-mod-map)

  (with-temp-buffer
    (insert-file tag-unify/global-tags-location)
    (goto-char (point-min))
    (while (< (point) (point-max))
      ((lambda (x) (puthash (car x) (string-to-number (cadr x)) tag-unify/global-tags)) (split-string (buffer-substring (line-beginning-position) (line-end-position)) ":" nil " "))
      (forward-line)
      )
    )

  (evil-define-operator tag-unify/tag-unify-helm-start (beg end)
    """ Opens the Tagging Helm """
    (interactive "<R>")
    (setq tag-unify/tag-unify-region `(,beg . ,(line-number-at-pos end)))
    (let* ((candidates (tag-unify/tag-unify-candidates))
           (main-source (cons `(candidates . ,(mapcar 'car candidates)) tag-unify/tag-unify-helm))
           )
      (helm :sources '(main-source tag-unify/tag-unify-fallback-source)
            :input "")
      ))

  )
(defun tag-unify/post-init-dired ()
  (spacemacs/set-leader-keys-for-major-mode 'dired-mode
    "c" 'tag-unify/clean-marked-files
    "!" 'tag-unify/chop-long-files-from-dired
    "B" 'tag-unify/unify-pdf-locations
    )
  )
(defun tag-unify/post-init-evil ()
  (evil-ex-define-cmd "t[ag]" 'tag-unify/tag-unify-helm-start)
  (evil-ex-define-cmd "to" 'tag-unify/tag-occurrences)
  (evil-ex-define-cmd "toa" 'tag-unify/tag-occurrences-in-open-buffers)
  (evil-ex-define-cmd "tv"  'org-tags-view)
  (evil-ex-define-cmd "ts"  'org-set-tags)
  )
(defun tag-unify/init-tag-clean-minor-mode ()
  (use-package tag-clean-minor-mode
    :defer t
    :commands (tag-clean-minor-mode)
    :config (progn
              (push 'tag-clean-minor-mode minor-mode-list)
              (spacemacs|define-transient-state tag-clean
                :title "Tag Cleaning Transient State"
                :doc (concat "
    | Commands   ^^|
    |------------^^|------------^^|
    | [_q_] Quit   | [_!_] Split  |
    | [_f_] Filter | [_p_] Prev   |
    | [_s_] Sub    | [_l_] Leave  |
")
                :bindings
                ("q" nil :exit t)
                ("f" tag-clean/mark-to-filter)
                ("s" tag-clean/mark-to-sub)
                ("p" tag-clean/previous)
                ("l" tag-clean/leave)
                ("!" tag-unify/org-split-on-headings :exit t)
                )
              (spacemacs/set-leader-keys-for-minor-mode 'tag-clean-minor-mode
                "." 'spacemacs/tag-clean-transient-state/body
                )
              )
    )
  )
