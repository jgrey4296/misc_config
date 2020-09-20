;; helm actions

(defun jg-tag-open-url-action (x)
  " An action added to helm-grep for loading urls found in bookmarks "
  (let* ((marked (helm-marked-candidates))
         (no-props (mapcar (lambda (x) (substring-no-properties x 0 (length x))) marked))
         link-start-point
         )
    (with-temp-buffer
      (mapc (lambda (x) (insert (format "%s\n" x))) no-props)
      (goto-char (point-min))
      (while (not (equal (point) (point-max)))
        ;; (search-forward "href=\"")
        ;; (setq link-start-point (point))
        ;; (search-forward "\" TAGS")
        ;; (backward-char (length "\" TAGS"))
        (message "Opening: %s" (buffer-substring (line-beginning-position) (line-end-position)))
        (goto-char (line-beginning-position))
        (+jg-org-open_link_externally)
        (forward-line)
        )
      )
    )
  )
(defun jg-tag-tweet-link-action (candidate)
  "Helm action to open a tweet buffer with the link inserted"
  (evil-window-new (get-buffer-window helm-current-buffer)
                   "*Link Tweeting*")
  (set (make-local-variable 'backup-inhibited) t)
  (auto-save-mode -1)
  (evil-window-set-height 10)
  (evil-initialize-local-keymaps)
  (evil-local-set-key 'normal
                      (kbd "C-c C-C") 'jg-tag-tweet-link-finish)
  (insert "\n")
  (insert candidate)
  (redraw-display)
  )
(defun jg-tag-tweet-link-finish ()
  "Action to finish and tweet a link"
  (interactive)
  (let* ((text (buffer-substring-no-properties (point-min) (point-max))))
    (+jg-twitter-twitter-tweet-text text nil '(+jg-twitter-tweet_sentinel))
    ))
(defun jg-tag-org-set-tags (x)
  """ Improved action to add and remove tags Toggle Selected Tags
Can operate on regions of headings """
  (let* ((visual-candidates (helm-marked-candidates))
         (actual-candidates (mapcar (lambda (x) (cadr (assoc x jg-tag-unify-layer-candidates-names))) visual-candidates))
         (prior-point 1)
         (end-pos jg-tag-unify-layer-marker)
         (current-tags '())
         (add-func (lambda (candidate)
                     (if (not (-contains? current-tags candidate))
                         (progn
                           (push candidate current-tags)
                           (puthash candidate 1 jg-tag-global-tags))
                       (progn
                         (setq current-tags (remove candidate current-tags))
                         (puthash candidate (- (gethash candidate jg-tag-global-tags) 1) jg-tag-global-tags))
                       ))))
    (save-excursion
      (setq prior-point (- (point) 1))
      (while (and (/= prior-point (point)) (< (point) end-pos))
        (progn (setq current-tags (org-get-tags nil t)
                     prior-point (point))
               (mapc add-func actual-candidates)
               (org-set-tags current-tags)
               (org-forward-heading-same-level 1)
               )))))
(defun jg-tag-insert-candidates (x)
  "A Helm action to insert selected candidates into the current buffer "
  (let ((candidates (helm-marked-candidates)))
    (with-helm-current-buffer
      ;;Substring -2 to chop off separating marks
      (insert (mapconcat (lambda (x) (substring x 0 -2)) candidates "\n")))))
(defun jg-tag-insert-links (x)
  "Helm action to insert selected candidates formatted as org links"
  (let ((candidates (helm-marked-candidates)))
    (with-helm-current-buffer
      ;;substring -2 to chop off separating marks
      (insert (mapconcat (lambda (x) (format "[[%s][%s]]" (substring x 0 -2) (substring x 0 -2))) candidates "\n")))))
(defun jg-tag-grep-filter-one-by-one (candidate)
  "A Grep modification for bookmark helm to extract a bookmark's url and tags"
  (if (consp candidate)
      ;; Already computed do nothing (default as input).
      candidate
    (let* ((line   (helm--ansi-color-apply candidate))
           (split  (helm-grep-split-line line))
           ;; Normalize Size of this:
           (lineno (nth 1 split))
           (norm_ln (s-append (s-repeat (- 6 (string-width lineno)) " ") lineno))
           ;; The Actual Line:
           (str    (nth 2 split))
           (sub    (substring str (or (s-index-of "HREF=" str) 0)))
           (tag_index (s-index-of "TAGS=" sub))
           (url (substring sub (string-width "HREF=\"") tag_index))
           (tags (substring sub (+ (string-width "HREF=\"") (or tag_index 0)) (s-index-of ">" sub)))
           (chopped_tags (substring tags 0 (min 50 (string-width tags))))
           (norm_tags (s-append (s-repeat (- 50 (string-width chopped_tags)) " ") chopped_tags))
           )
      (cons (concat (propertize norm_ln 'face 'helm-grep-lineno)
                    (propertize (concat ": " norm_tags) 'face 'rainbow-delimiters-depth-3-face)
                    (propertize (concat ": " url) 'face 'rainbow-delimiters-depth-1-face))
            (or url line))
      )
    )
  )
(defun jg-tag-find-file (x)
  "A simple helm action to open selected files"
  (let ((files (if (helm-marked-candidates) (helm-marked-candidates) (list x))))
    (mapc 'find-file (mapcar 'string-trim files))
    )
  )
(defun jg-tag-process-candidates (x)
  "Utility to tidy bibtex-completion-candidates for helm-bibtex"
  (cons (s-replace-regexp ",? +" " " (car x))
        (cdr x))
  )

(defun jg-tag-file-select-helm (candidates)
    " Given a list of Files, provide a helm to open them "
    (interactive)
    ;; (message "File Select Helm Candidates: %s" (helm-marked-candidates))
    ;;process candidates?
    (let*((all-candidates (if (helm-marked-candidates) (-flatten (helm-marked-candidates)) candidates))
          (source (cons `(candidates . ,all-candidates) jg-tag-file-select-source)))
      (helm :sources source
            :full-frame t
            :buffer "*helm file select*"
            )
      )
    )
(defun jg-tag-helm-twitter ()
    "Run a Helm for searching twitter users"
    (interactive)
    ;;if twitter info not loaded, load
    (if (null jg-tag-twitter-helm-candidates)
        (with-temp-buffer
          (setq jg-tag-twitter-helm-candidates '())
          (insert-file jg-tag-twitter-account-index)
          (goto-char (point-min))
          (let (curr)
            (while (< (point) (point-max))
              (setq curr (split-string (buffer-substring (point) (line-end-position)) ":"))
              (push `(,(car curr) . ,(cdr curr)) jg-tag-twitter-helm-candidates)
              (forward-line)
              )
            )
          )
      )
    ;;add candidates to source
    (let ((source (cons `(candidates . jg-tag-twitter-helm-candidates) jg-tag-twitter-helm-source)))
      ;;call helm
      (helm :sources source
            :full-frame t
            :buffer "*helm twitter*"
            :truncate-lines t
            ;;TODO: is this necessary?
            :candidates jg-tag-twitter-helm-candidates
            )
      )
    )
(defun jg-tag-helm-heading-twitter ()
    "Run a Helm for searching twitter users"
    (interactive)
    ;;if twitter info not loaded, load
    (if (null jg-tag-twitter-heading-helm-candidates)
        (with-temp-buffer
          (setq jg-tag-twitter-heading-helm-candidates '())
          (insert-file jg-tag-twitter-tag-index)
          (goto-char (point-min))
          (let (curr)
            (while (< (point) (point-max))
              (setq curr (split-string (buffer-substring (point) (line-end-position)) ":"))
              (push `(,(car curr) . ,(cdr curr)) jg-tag-twitter-heading-helm-candidates)
              (forward-line)
              )
            )
          )
      )
    ;;add candidates to source
    (let ((source (cons `(candidates . jg-tag-twitter-heading-helm-candidates) jg-tag-twitter-helm-source)))
      ;;call helm
      (helm :sources source
            :full-frame t
            :buffer "*helm twitter heading*"
            :truncate-lines t
            ;;TODO: is this necessary?
            :candidates jg-tag-twitter-heading-helm-candidates
            )
      )
    )
(defun jg-tag-helm-bookmarks ()
    " Run a Helm for search and opening html bookmarks "
    (interactive)
    (helm-set-local-variable
     'helm-grep-include-files (format "--include=%s" jg-tag-loc-bookmarks)
     'helm-grep-last-targets `(,jg-tag-loc-bookmarks)
     'default-directory "~/github/writing/resources/"
     )
    (helm :sources jg-tag-bookmark-helm-source
          :full-frame t
          :buffer "*helm bookmarks*"
          :truncate-lines t
          )
    )
(defun jg-tag-helm-unified (arg)
    (interactive "P")
    ;;Clear Cache if necessary
    (when arg
      (bibtex-completion-clear-cache))
    ;;Load headings if necessary
    (if (null jg-tag-twitter-heading-helm-candidates)
        (with-temp-buffer
          (setq jg-tag-twitter-heading-helm-candidates '())
          (insert-file jg-tag-twitter-tag-index)
          (goto-char (point-min))
          (let (curr)
            (while (< (point) (point-max))
              (setq curr (split-string (buffer-substring (point) (line-end-position)) ":"))
              (push `(,(car curr) . ,(cdr curr)) jg-tag-twitter-heading-helm-candidates)
              (forward-line)
              )
            )
          )
      )
    ;;Load twitter users if necessary
    (if (null jg-tag-twitter-helm-candidates)
        (with-temp-buffer
          (setq jg-tag-twitter-helm-candidates '())
          (insert-file jg-tag-twitter-account-index)
          (goto-char (point-min))
          (let (curr)
            (while (< (point) (point-max))
              (setq curr (split-string (buffer-substring (point) (line-end-position)) ":"))
              (push `(,(car curr) . ,(cdr curr)) jg-tag-twitter-helm-candidates)
              (forward-line)
              )
            )
          )
      )
    ;;Set local variables for bookmarks
    (helm-set-local-variable
     'helm-grep-include-files (format "--include=%s" jg-tag-loc-bookmarks)
     'helm-grep-last-targets `(,jg-tag-loc-bookmarks)
     'default-directory "~/github/writing/resources/"
     )

    ;;add candidates to source
    (let* ((bibtex-completion-additional-search-fields '("tags" "year"))
           (candidates-bibtex (if (or arg (null jg-tag-helm-bibtex-candidates))
                                  (progn (message "Generating Candidates")
                                         (bibtex-completion-init)
                                         (setq jg-tag-helm-bibtex-candidates
                                               (mapcar 'jg-tag-process-candidates (bibtex-completion-candidates)))
                                         jg-tag-helm-bibtex-candidates)
                                jg-tag-helm-bibtex-candidates
                                ))
           (source-tw (cons `(candidates . jg-tag-twitter-helm-candidates) jg-tag-twitter-helm-source))
           (source-heading (cons `(candidates . jg-tag-twitter-heading-helm-candidates) jg-tag-twitter-helm-source)))
      ;;call helm
      (helm :sources '(source-heading jg-tag-helm-source-bibtex jg-tag-bookmark-helm-source )
            :full-frame t
            :buffer "*Helm unified*"
            :truncate-lines t
            :bibtex-candidates candidates-bibtex
            )
      )
    )

(after! helm
  (setq helm-grep-actions (append helm-grep-actions '(("Open Url" . jg-tag-open-url-action))))
  ;; Build a Custom grep for bookmarks
  (setq jg-tag-bookmark-helm-source
        (helm-make-source "Bookmark Helm" 'helm-grep-class
          :action (helm-make-actions "Open Url" 'jg-tag-open-url-action
                                     "Insert"   'jg-tag-insert-candidates
                                     "Insert Link" 'jg-tag-insert-links
                                     "Tweet Link"  'jg-tag-tweet-link-action
                                     )
          :filter-one-by-one 'jg-tag-grep-filter-one-by-one
          :nomark nil
          :backend helm-grep-default-command
          :pcre nil
          )
        jg-tag-twitter-helm-source
        (helm-make-source "Twitter Helm" 'helm-source
          :action (helm-make-actions "File Select Helm" 'jg-tag-file-select-helm)
          )
        jg-tag-unify-lauyer/twitter-heading-helm-source
        (helm-make-source "Twitter Heading Helm" 'helm-source
          :action (helm-make-actions "File Select Helm" 'jg-tag-file-select-helm)
          )
        jg-tag-file-select-source
        (helm-make-source "Twitter File Select Helm" 'helm-source
          :action (helm-make-actions "Find File" 'jg-tag-find-file)
          )

        jg-tag-helm
        (helm-make-source "Helm Tagging" 'helm-source
          :action (helm-make-actions "Set" 'jg-tag-set-tags))

        jg-tag-fallback-source
        (helm-make-source "Helm Fallback Source" 'helm-source
          :action (helm-make-actions "Create" 'jg-tag-set-new-tag)
          :filtered-candidate-transformer (lambda (_c _s) (list helm-pattern)))
          )

  (map! :leader
        (:prefix ("ah" . "Helms")
         "f" 'jg-tag-helm-bookmarks
         "t" 'jg-tag-helm-twitter
         "h" 'jg-tag-helm-heading-twitter
         "u" 'jg-tag-helm-unified
         )
        )


)
(after! helm-bibtex
  ;; Keybind my bib helm
  (map! "a h b" #'jg-tag-helm-bibtex)

  ;; Define the bib helm
  (defvar jg-tag-helm-source-bibtex
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
  (defun jg-tag-helm-bibtex (&optional arg local-bib input)
    " Custom implementation of helm-bibtex"
    (interactive "P")
    (require 'helm-bibtex)
    (when arg
      (bibtex-completion-clear-cache))
    (let* ((bibtex-completion-additional-search-fields '("tags" "year"))
           (candidates (if (or arg (null jg-tag-helm-bibtex-candidates))
                           (progn (message "Generating Candidates")
                                  (bibtex-completion-init)
                                  (setq jg-tag-helm-bibtex-candidates
                                        (mapcar 'jg-tag-process-candidates (bibtex-completion-candidates)))
                                  jg-tag-helm-bibtex-candidates)
                         jg-tag-helm-bibtex-candidates
                         ))
           )
      (helm :sources `(,jg-tag-helm-source-bibtex)
            :full-frame helm-bibtex-full-frame
            :buffer "*helm bibtex*"
            :input input
            :bibtex-local-bib local-bib
            :bibtex-candidates candidates
            )))
  )

(after! (f helm-bibtex)
  (jg-tag-build-bibtex-list)
)
