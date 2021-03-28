;; helm actions
(defun +jg-tag-open-url-action (x)
  " An action added to helm-grep for loading urls found in bookmarks "
  (let* ((marked (helm-marked-candidates))
         (no-props (mapcar (lambda (x) (plist-get x :url)) marked))
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
(defun +jg-tag-tweet-link-action (candidate)
  "Helm action to open a tweet buffer with the link inserted"
  (evil-window-new (get-buffer-window helm-current-buffer)
                   "*Link Tweeting*")
  (set (make-local-variable 'backup-inhibited) t)
  (auto-save-mode -1)
  (evil-window-set-height 10)
  (evil-initialize-local-keymaps)
  (evil-local-set-key 'normal
                      (kbd "C-c C-C") '+jg-tag-tweet-link-finish)
  (insert "\n")
  (insert (plist-get candidate :url))
  (redraw-display)
  )
(defun +jg-tag-tweet-link-finish ()
  "Action to finish and tweet a link"
  (interactive)
  (let* ((text (buffer-substring-no-properties (point-min) (point-max))))
    (+jg-twitter-twitter-tweet-text text nil '(+jg-twitter-tweet_sentinel))
    ))
(defun +jg-tag-insert-candidates (x)
  "A Helm action to insert selected candidates into the current buffer "
  (let ((candidates (helm-marked-candidates)))
    (with-helm-current-buffer
      ;;Substring -2 to chop off separating marks
      (insert (mapconcat (lambda (x) (substring x 0 -2)) candidates "\n")))))
(defun +jg-tag-insert-bookmarks (x)
 (let ((candidates (helm-marked-candidates)))
    (with-helm-current-buffer
      ;;Substring -2 to chop off separating marks
      (insert (mapconcat (lambda (x) (concat (plist-get x :url)
                                             " : \n"
                                             (plist-get x :tags)))
                         candidates "\n\n")))))
(defun +jg-tag-insert-links (x)
  "Helm action to insert selected candidates formatted as org links"
  (let ((candidates (helm-marked-candidates)))
    (with-helm-current-buffer
      ;;substring -2 to chop off separating marks
      (insert (mapconcat (lambda (x) (format "[[%s][%s]]    : %s" (plist-get x :url) (plist-get x :url) (plist-get x :tags))) candidates "\n")))))
(defun +jg-tag-grep-filter-one-by-one (candidate)
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
           (tag_index (s-index-of "TAGS=\"" sub))
           (url (substring sub (string-width "HREF=\"") (- tag_index 2)))
           (tags (substring sub (+ (string-width "HREF=\"") (or tag_index 0)) (s-index-of "\">" sub)))
           (chopped_tags (substring tags 0 (min 50 (string-width tags))))
           (norm_tags (s-append (s-repeat (- 50 (string-width chopped_tags)) " ") chopped_tags))
           )
      `(,(concat (propertize norm_ln 'face 'helm-grep-lineno)
                 (propertize (concat ": " norm_tags) 'face 'rainbow-delimiters-depth-3-face)
                 (propertize (concat ": " url) 'face 'rainbow-delimiters-depth-1-face))
        :url ,url
        :tags ,tags
        :line ,line
        )
      )
    )
  )
(defun +jg-tag-find-file (x)
  "A simple helm action to open selected files"
  (let ((files (if (helm-marked-candidates) (helm-marked-candidates) (list x))))
    (mapc 'find-file (mapcar 'string-trim files))
    )
  )

(defun +jg-tag-insert-twitter-link (candidate)
  (let ((candidates (mapcar 'car (helm-marked-candidates))))
    (with-helm-current-buffer
      (loop for entry in candidates
            do (let ((name (substring entry 1)))
                 (insert (format "[[https://twitter.com/%s][%s]]\n" name entry)))))))

;; Dual Helm / Helm-Action:
(defun +jg-tag-file-select-helm (candidates)
    " Given a list of Files, provide a helm to open them "
    (interactive)
    ;;(message "File Select Helm Candidates: %s" (helm-marked-candidates))
    ;;process candidates?
    (let*(;;(candidate-names (mapcar 'car (helm-marked-candidates)))
          (candidate-values (mapcar 'cdr (helm-marked-candidates)))
          (all-candidates (-flatten candidate-values))
          (source (cons `(candidates . ,all-candidates) jg-tag-file-select-source)))
      (helm :sources source
            :full-frame t
            :buffer "*helm file select*"
            )
      )
    )

;; Helm Activators:
(defun +jg-tag-helm-twitter ()
    "Run a Helm for searching twitter users"
    (interactive)
    ;;if twitter info not loaded, load
    (if (null jg-tag-twitter-helm-candidates)
        (with-temp-buffer
          (setq jg-tag-twitter-helm-candidates '())
          (insert-file jg-tag-loc-twitter-account-index)
          (goto-char (point-min))
          (let (curr)
            (while (< (point) (point-max))
              (setq curr (split-string (buffer-substring (point) (line-end-position)) ":" t "\s+"))
              (push `(,(car curr) . ,curr) jg-tag-twitter-helm-candidates)
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
            )
      )
    )
(defun +jg-tag-helm-heading-twitter ()
    "Run a Helm for searching twitter users"
    (interactive)
    ;;if twitter info not loaded, load
    (if (null jg-tag-twitter-heading-helm-candidates)
        (with-temp-buffer
          (setq jg-tag-twitter-heading-helm-candidates '())
          (insert-file jg-tag-loc-twitter-tag-index)
          (goto-char (point-min))
          (let (curr)
            (while (< (point) (point-max))
              (setq curr (split-string (buffer-substring (point) (line-end-position)) ":" t "\s+"))
              (push `(,(car curr) . ,curr) jg-tag-twitter-heading-helm-candidates)
              (forward-line)
              )
            )
          )
      )
    ;;add candidates to source
    (let ((source (cons `(candidates . jg-tag-twitter-heading-helm-candidates) jg-tag-twitter-heading-helm-source)))
      ;;call helm
      (helm :sources source
            :full-frame t
            :buffer "*helm twitter heading*"
            :truncate-lines t
            )
      )
    )
(defun +jg-tag-helm-bookmarks ()
    " Run a Helm for search and opening html bookmarks "
    (interactive)
    (helm-set-local-variable
     'helm-grep-include-files (format "--include=%s" jg-tag-loc-bookmarks)
     'helm-grep-last-targets `(,jg-tag-loc-bookmarks)
     'default-directory jg-tag-loc-default-helm-directory
     )
    (helm :sources jg-tag-bookmark-helm-source
          :full-frame t
          :buffer "*helm bookmarks*"
          :truncate-lines t
          )
    )
(defun +jg-tag-helm-unified ()
    (interactive)
    ;;Load headings if necessary
    (if (null jg-tag-twitter-heading-helm-candidates)
        (with-temp-buffer
          (setq jg-tag-twitter-heading-helm-candidates '())
          (insert-file jg-tag-loc-twitter-tag-index)
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
          (insert-file jg-tag-loc-twitter-account-index)
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
     'default-directory jg-tag-loc-default-helm-directory
     )
    ;;add candidates to source
    (let* ((source-tw (cons `(candidates . jg-tag-twitter-helm-candidates) jg-tag-twitter-helm-source))
           (source-heading (cons `(candidates . jg-tag-twitter-heading-helm-candidates) jg-tag-twitter-heading-helm-source)))
      ;;call helm
      (helm :sources '(source-heading jg-tag-bookmark-helm-source)
            :full-frame t
            :buffer "*Helm unified*"
            :truncate-lines t
            )
      )
    )
(defun +jg-tag-helm-tagger (&optional beg end)
    """ Opens the Tagging Helm """
    (set-marker jg-tag-marker (if (eq evil-state 'visual)  evil-visual-end (line-end-position)))
    (let* ((candidates (+jg-tag-candidates))
           (main-source (cons `(candidates . ,candidates) jg-tag-helm))
           )
      (helm :sources '(main-source jg-tag-fallback-source)
            :input "")
      )
    )



;; Setup
(after! helm-files
  (setq helm-grep-actions (append helm-grep-actions '(("Open Url" . jg-tag-open-url-action))))
  ;; Build a Custom grep for bookmarks
  (setq jg-tag-bookmark-helm-source
        (helm-make-source "Bookmark Helm" 'helm-grep-class
          :action (helm-make-actions "Open Url" '+jg-tag-open-url-action
                                     "Insert"   '+jg-tag-insert-bookmarks
                                     "Insert Link" '+jg-tag-insert-links
                                     "Tweet Link"  '+jg-tag-tweet-link-action
                                     )
          :filter-one-by-one '+jg-tag-grep-filter-one-by-one
          :nomark nil
          :backend "grep --color=always -a -d skip %e -n%cH -e %p %f"
          :pcre nil
           ))
)
(after! helm
  (setq jg-tag-twitter-helm-source
        (helm-make-source "Twitter Helm" 'helm-source
          :action (helm-make-actions "File Select Helm" '+jg-tag-file-select-helm
                                     "Insert User Link" '+jg-tag-insert-twitter-link)
          )
        ;; ==========
        jg-tag-twitter-heading-helm-source
        (helm-make-source "Twitter Heading Helm" 'helm-source
          :action (helm-make-actions "File Select Helm" '+jg-tag-file-select-helm)
          )
        ;; ==========
        jg-tag-file-select-source
        (helm-make-source "Twitter File Select Helm" 'helm-source
          :action (helm-make-actions "Find File" '+jg-tag-find-file)
          )
        ;; ==========
        jg-tag-helm
        (helm-make-source "Helm Tagging" 'helm-source
          :action (helm-make-actions "Set" '+jg-tag-set-tags))
        ;; ==========
        jg-tag-fallback-source
        (helm-make-source "Helm Fallback Source" 'helm-source
          :action (helm-make-actions "Create" '+jg-tag-set-new-tag)
          :filtered-candidate-transformer (lambda (_c _s) (list helm-pattern)))
          )

)
