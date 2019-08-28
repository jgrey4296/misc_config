(defun tag-unify/process-candidates (x)
  (cons (s-replace-regexp ",? +" " " (car x))
        (cdr x))
  )
(defun tag-unify/build-bibtex-list ()
  (mapcar (lambda (x) (f-join tag-unify/loc-bibtex x))
          (-filter (lambda (x) (s-equals? (f-ext x) "bib"))
                   (directory-files tag-unify/loc-bibtex))))
(defun tag-unify/rebuild-tag-database ()
  ;; regenerate the master list of tags
  ;; from bookmarks



  ;; and from bibtex





  )
(defun tag-unify/get-bibtex-entries (x)
  ;;given a candidate tag name,
  ;;find all bibtex entries that match
  )
(defun tag-unify/load-bookmark-entry (x)
  ;;given a candidate tag name,
  ;;find all bookmark entries that match



  )
(defun tag-unify/open-url-action (x)
  """ An action added to helm-grep for loading urls found
  in bookmarks "
  (interactive)
  (let* ((marked (helm-marked-candidates))
         (no-props (mapcar (lambda (x) (substring-no-properties x 0 (length x))) marked))
         link-start-point
         )
    (with-temp-buffer
      (mapcar (lambda (x) (insert (format "%s\n" x))) no-props)
      (goto-char (point-min))
      (while (not (equal (point) (point-max)))
        ;; (search-forward "href=\"")
        ;; (setq link-start-point (point))
        ;; (search-forward "\" TAGS")
        ;; (backward-char (length "\" TAGS"))
        (message "Opening: %s" (buffer-substring (line-beginning-position) (line-end-position)))
        (goto-char (line-beginning-position))
        (jg_layer/open_link_externally)
        (forward-line)
        )
      )
    )
  )
(defun tag-unify/insert-candidates (x)
  (let ((candidates (helm-marked-candidates)))
    (with-helm-current-buffer
      (insert (mapconcat (lambda (x) (substring x 0 -2)) candidates "\n")))))

(defun tag-unify/insert-links (x)
  (let ((candidates (helm-marked-candidates)))
    (with-helm-current-buffer
      (insert (mapconcat (lambda (x) (format "[[%s][%s]]" (substring x 0 -2) (substring x 0 -2))) candidates "\n")))))

(defun tag-unify/tweet-link-action (candidate)
  (evil-window-new (get-buffer-window helm-current-buffer)
                   "*Link Tweeting*")
  (set (make-local-variable 'backup-inhibited) t)
  (auto-save-mode -1)
  (evil-window-set-height 10)
  (evil-initialize-local-keymaps)
  (evil-local-set-key 'normal
                      (kbd "C-c C-C") 'tag-unify/tweet-link-finish)
  (insert "\n")
  (insert candidate)
  (redraw-display)
  )

(defun tag-unify/tweet-link-finish ()
  (interactive)
  (let* ((text (buffer-substring-no-properties (point-min) (point-max))))
    (jg_twitter/twitter-tweet-text text nil '(jg_twitter/tweet_sentinel))
    ))

(defun tag-unify/grep-filter-one-by-one (candidate)
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
                    ": " norm_tags
                    (propertize (concat ": " url) 'face 'term-color-blue))
            (or url line))
      )
    )
  )
