;;; +actions.el -*- lexical-binding: t; -*-

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

(defun +jg-tag-insert-bookmarks (x)
 (let ((candidates (helm-marked-candidates)))
    (with-helm-current-buffer
      ;;Substring -2 to chop off separating marks
      (insert (mapconcat (lambda (x) (plist-get x :url))
                         candidates "\n\n")))))

(defun +jg-tag-insert-links (x)
  "Helm action to insert selected candidates formatted as org links"
  (let ((candidates (helm-marked-candidates)))
    (with-helm-current-buffer
      ;;substring -2 to chop off separating marks
      (insert (mapconcat (lambda (x) (format "[[%s][%s]]    : %s" (plist-get x :url) (plist-get x :url) (plist-get x :tags))) candidates "\n")))))

(defun +jg-tag-find-file (x)
  "A simple helm action to open selected files"
  (let ((files (if (helm-marked-candidates) (helm-marked-candidates) (list x))))
    (mapc 'find-file (mapcar 'string-trim files))
    )
  )

(defun +jg-tag-insert-twitter-link (candidate)
  (let ((candidates (mapcar 'car (helm-marked-candidates))))
    (with-helm-current-buffer
      (cl-loop for entry in candidates
            do (let ((name (substring entry 1)))
                 (insert (format "[[https://twitter.com/%s][%s]]\n" name entry)))))))

(defun +jg-tag-file-select-helm (candidates)
    " Given a list of Files, provide a helm to open them "
    (interactive)
    ;;(message "File Select Helm Candidates: %s" (helm-marked-candidates))
    ;;process candidates?
    (let*(;;(candidate-names (mapcar 'car (helm-marked-candidates)))
          (candidate-values (helm-marked-candidates))
          (all-candidates (-flatten (mapcar (lambda (x) (plist-get x :files)) candidate-values)))
          (source (cons `(candidates . ,all-candidates) jg-tag-file-select-source)))
      (helm :sources source
            :full-frame t
            :buffer "*helm file select*"
            )
      )
    )
