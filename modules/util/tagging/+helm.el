;; -*- mode: emacs-lisp; lexical-binding: t; -*-

;;-- utils
(defun +jg-tag-clean-input (x)
  (let ((trimmed (string-trim x)))
    (s-replace-regexp "\s+" "_" trimmed)
    )
  )

(defun +jg-tag-save-helm-buffer ()
  (interactive)
  (let ((results (with-helm-buffer (buffer-string))))
    (helm-exit-and-execute-action
     #'(lambda (x)
         (with-temp-buffer-window "TestBuffer" 'display-buffer-pop-up-frame nil
           (princ results)
           )
         )
     )
    )
  )
;;-- end utils

;;-- actions
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
(defun +jg-tag-set-tags-re-entrant (x)
  (+jg-tag-set-tags x)
  (with-helm-buffer
    (setq-local helm-input-local " ")
  )
  (helm-resume jg-tag-helm-buffer-name)
  )
(defun +jg-tag-set-new-tag-re-entrant (x)
  (+jg-tag-set-new-tag x)
  (with-helm-buffer
    (setq-local helm-input-local " ")
  )
  (helm-resume jg-tag-helm-buffer-name)
  )
;;-- end actions

;;-- dual helm/action
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
(defun +jg-tag-file-display (candidates)
  (interactive)
  (let*((candidates (plist-get (car (helm-marked-candidates)) :files)))
    (with-temp-buffer-window "Helm Twitter Grep Results"
        'display-buffer-pop-up-window nil
      (mapcar (lambda (x) (princ x) (princ "\n")) candidates)
      )
    )
  )

;;-- end dual helm/action

;;-- candidate transformers
(defun +jg-tag-sort-by-files (candidates source)
  (sort candidates (lambda (a b)
                     (let ((a-count (plist-get (cdr a) :count))
                           (b-count (plist-get (cdr b) :count)))
                       (> (if (stringp a-count) (string-to-number a-count) a-count)
                          (if (stringp b-count) (string-to-number b-count) b-count))
                       ))))
(defun +jg-tag-helm-index-file-transformer (cands)
  (let* ((as-list (mapcar (lambda (x) (split-string x ":" t "\s+")) cands))
         (max-tag (apply 'max (mapcar (lambda (x) (length (car x))) as-list)))
         (has-count (and (car as-list) (cadr (car as-list)) (s-numeric? (cadr (car as-list))))))
    (mapcar (lambda (x)
              `(,(format "%s%s: %s" (car x) (s-repeat (+ 1 (- max-tag (length (car x)))) " ") (if has-count (cadr x)
                                                                                                (length (cdr x))))
                . (:count ,(if has-count (cadr x) (length (cdr x)))
                   :files ,(if has-count (cddr x) (cdr x)))))
            as-list)
    )
  )
(defun +jg-tag-grep-filter-one-by-one (candidate)
        "A Grep modification for bookmark helm to extract a bookmark's url and tags"
        (if (consp candidate)
            ;; Already computed do nothing (default as input).
            candidate
          (let* ((line   (ansi-color-apply candidate))
                 (split  (helm-grep-split-line line)))
            (if (and split (>= (length split) 2))
                (let* ((lineno (if (nth 1 split) (nth 1 split) "1"))                 ;; Normalize Size of this
                       (norm-ln (s-append (s-repeat (- 6 (string-width lineno)) " ") lineno))
                       (str    (nth 2 split))                                        ;; The Actual Line:
                       (sub    str)                                                  ;;(substring str (or (s-index-of "HREF=" str) 0)))
                       (tag-index (s-index-of " :" sub))                             ;;(s-index-of "TAGS=\"" sub))
                       (url (substring sub 0 tag-index))                             ;;(string-width "HREF=\"") (- tag_index 2)))
                       (tags (substring sub (+ tag-index 2) nil))
                       (chopped_tags (substring tags 0 (min 100 (string-width tags)))) ;; Normalize the lengths of tags so urls are aligned
                       (norm-tags (s-append (s-repeat (- 100 (string-width chopped_tags)) " ") chopped_tags))
                       )
                  `(,(concat (propertize norm-ln 'face 'helm-grep-lineno)
                             (propertize (concat ": " norm-tags) 'face 'rainbow-delimiters-depth-3-face)
                             (propertize (concat ": " url) 'face 'rainbow-delimiters-depth-1-face))
                    :url ,url
                    :tags ,tags
                    :line ,line
                    )
                  )
              )
            )
          )
        )
(defun +jg-tag-twitter-grep-filter-one-by-one (candidate)
        "A Grep modification for twitter grep helm to extract information correctly "
        (if (consp candidate)
            ;; Already computed do nothing (default as input).
            candidate
          (let* ((line   (ansi-color-apply candidate))
                 (split  (helm-grep-split-line line))
                 ;; Normalize Size of this:
                 (lineno (if (nth 1 split) (nth 1 split) "1"))
                 (norm-ln (s-append (s-repeat (- 6 (string-width lineno)) " ") lineno))
                 ;; The Actual Line:
                 (str    (nth 2 split))
                 (sub    str) ;;(substring str (or (s-index-of "HREF=" str) 0)))
                 (tag-index (s-index-of " :" sub)) ;;(s-index-of "TAGS=\"" sub))
                 (tag (substring sub 0 tag-index)) ;;(string-width "HREF=\"") (- tag_index 2)))
                 (file-str (substring sub (+ tag-index 2) nil))
                 (file-list (split-string file-str ":" t "\s+"))
                 (has-count (s-numeric? (car file-list)))
                 (file-count (if has-count (string-to-number (car file-list)) (length file-list)))
                 (files (if has-count (cdr file-list) file-list))
                 ;; Normalize the lengths of tags so urls are aligned
                 (chopped_files (substring file-str (min 100 (string-width file-str))))
                 (norm-files (s-append (s-repeat (- 100 (string-width chopped_files)) " ") chopped_files))
                 )
            `(,(concat (propertize norm-ln 'face 'helm-grep-lineno)
                       (propertize (concat ": " tag) 'face 'rainbow-delimiters-depth-3-face)
                       (propertize (concat ": " (number-to-string file-count)) 'face 'rainbow-delimiters-depth-4-face))
              . (:count ,(if has-count (string-to-number (car file-list)) (length file-list))
                 :files ,(if has-count (cdr file-list) file-list))
              )
            )
          )
        )
;;-- end candidate transformers

;;-- pattern transformers
(defun +jg-tag-bookmark-helm-pattern-transformer (pattern)
  pattern
)
(defun +jg-tag-grep-pattern-transformer (pattern)
  (format "^[^:]*%s[^:]* :" pattern))
;;-- end pattern transformers

;;-- helms
(defun +jg-tag-helm-tag-twitter ()
    "Run a Helm for searching twitter tags"
    (interactive)
    (helm :sources jg-tag-twitter-tag-helm-source
          :full-frame t
          :buffer "*helm twitter*"
          :truncate-lines t
          )
    )
(defun +jg-tag-helm-twitter-grep (arg)
  (interactive "p")
    (helm-set-local-variable
     'helm-grep-include-files (format "--include=%s" jg-tag-loc-twitter-grep-index)
     'helm-grep-last-targets `(,jg-tag-loc-twitter-grep-index)
     'default-directory jg-tag-loc-default-helm-directory
     )
    (helm :sources (if (eq arg 4)
                       jg-tag-twitter-grep-helm-source-alt
                     jg-tag-twitter-grep-helm-source)
          :full-frame t
          :buffer "*helm grep twitter*"
          :truncate-lines t
          )
    )
(defun +jg-tag-helm-account-twitter ()
    "Run a Helm for searching twitter users"
    (interactive)
    (helm :sources jg-tag-twitter-account-helm-source
          :full-frame t
          :buffer "*helm twitter heading*"
          :truncate-lines t
          )
    )
(defun +jg-tag-helm-bookmarks ()
    " Run a Helm for search and opening html bookmarks "
    (interactive)
    (helm-set-local-variable
     ;; 'helm-grep-include-files (format "--include=%s" jg-tag-loc-bookmarks)
     'helm-grep-last-targets `(,jg-tag-loc-bookmarks)
     'default-directory jg-tag-loc-default-helm-directory
     'helm-grep-default-command (alist-get 'backend jg-tag-bookmark-helm-source)
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
    (if (null jg-tag-twitter-account-helm-candidates)
        (with-temp-buffer
          (setq jg-tag-twitter-account-helm-candidates '())
          (insert-file jg-tag-loc-twitter-account-index)
          (goto-char (point-min))
          (let (curr)
            (while (< (point) (point-max))
              (setq curr (split-string (buffer-substring (point) (line-end-position)) ":"))
              (push `(,(car curr) . ,(cdr curr)) jg-tag-twitter-account-helm-candidates)
              (forward-line)
              )
            )
          )
      )
    ;;Load twitter users if necessary
    (if (null jg-tag-twitter-tag-helm-candidates)
        (with-temp-buffer
          (setq jg-tag-twitter-tag-helm-candidates '())
          (insert-file jg-tag-loc-twitter-tag-index)
          (goto-char (point-min))
          (let (curr)
            (while (< (point) (point-max))
              (setq curr (split-string (buffer-substring (point) (line-end-position)) ":"))
              (push `(,(car curr) . ,(cdr curr)) jg-tag-twitter-tag-helm-candidates)
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
    (let* ((source-tw (cons `(candidates . jg-tag-twitter-tag-helm-candidates) jg-tag-twitter-tag-helm-source))
           (source-heading (cons `(candidates . jg-tag-twitter-account-helm-candidates) jg-tag-twitter-account-helm-source)))
      ;;call helm
      (helm :sources (list source-heading jg-tag-bookmark-helm-source)
            :full-frame t
            :buffer "*Helm unified*"
            :truncate-lines t
            )
      )
    )
(defun +jg-tag-helm-tagger (&optional beg end)
  " Opens the Tagging Helm "
  (set-marker jg-tag-marker (if (eq evil-state 'visual)  evil-visual-end (line-end-position)))
  (get-buffer-create jg-tag-helm-buffer-name)

  (let* ((current-tags (+jg-tag-get-tags))
         (candidates (+jg-tag-candidates current-tags))
         (main-source (cons `(candidates . ,candidates) jg-tag-helm-source))
         )
    (helm :sources (list main-source jg-tag-fallback-source)
          :input ""
          :buffer jg-tag-helm-buffer-name
          )
    )
  )
;;-- end helms

;;-- setup
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
          :filter-one-by-one   '+jg-tag-grep-filter-one-by-one
          :pattern-transformer '+jg-tag-bookmark-helm-pattern-transformer
          :nomark nil
          :backend "ggrep --color=always -a -d skip %e -n%cH -e %p %f"
          :pcre nil
          )
        )
  (setq jg-tag-twitter-grep-helm-source-alt
        (helm-make-source "twitter grep helm alt" 'helm-grep-class
          :action (helm-make-actions "File Select Helm" #'+jg-tag-file-select-helm
                                     "Display in Temp Buffer" #'+jg-tag-file-display)
          ;; :filtered-candidate-transformer '+jg-tag-grep-filter-candidate-transformer
          :filter-one-by-one '+jg-tag-twitter-grep-filter-one-by-one
          :filtered-candidate-transformer #'+jg-tag-sort-by-files
          :nomark nil
          :backend "ggrep --color=always -a -d skip %e -n%cH -e %p %f"
          :pattern-transformer '+jg-tag-grep-pattern-transformer
          :pcre nil
          ))
)

(after! helm
  (setq jg-tag-twitter-tag-helm-source
        (helm-build-in-file-source "Twitter Helm"
            jg-tag-loc-twitter-tag-index
          :action (helm-make-actions "File Select Helm" #'+jg-tag-file-select-helm
                                     "Insert User Link" #'+jg-tag-insert-twitter-link)
          :candidate-transformer #'+jg-tag-helm-index-file-transformer
          :filtered-candidate-transformer #'+jg-tag-sort-by-files
          )
        ;; ==========
        jg-tag-twitter-grep-helm-source
        (helm-build-in-file-source "Twitter Grep Helm"
            jg-tag-loc-twitter-grep-index
          :action (helm-make-actions "File Select Helm" #'+jg-tag-file-select-helm)
          :candidate-transformer #'+jg-tag-helm-index-file-transformer
          :pattern-transformer #'+jg-tag-grep-pattern-transformer
          :filtered-candidate-transformer #'+jg-tag-sort-by-files
          )
        ;; ==========
        jg-tag-twitter-account-helm-source
        (helm-build-in-file-source "Twitter Account Helm"
            jg-tag-loc-twitter-account-index
          :action (helm-make-actions "File Select Helm" #'+jg-tag-file-select-helm)
          :candidate-transformer #'+jg-tag-helm-index-file-transformer
          :pattern-transformer #'+jg-tag-grep-pattern-transformer
          :filtered-candidate-transformer #'+jg-tag-sort-by-files
          )
        ;; ==========
        jg-tag-file-select-source
        (helm-make-source "Twitter File Select Helm" 'helm-source
          :action (helm-make-actions "Find File" #'+jg-tag-find-file)
          )
        ;; ==========
        jg-tag-helm-source
        (helm-make-source "Helm Tagging" 'helm-source
          :action (helm-make-actions "Re-entrant-set" #'+jg-tag-set-tags-re-entrant
                                     "Set"            #'+jg-tag-set-tags)
          :pattern-transformer #'+jg-tag-clean-input
          )
        ;; ==========
        jg-tag-fallback-source
        (helm-build-dummy-source "Helm Tags Fallback Source"
          :action (helm-make-actions "Re-entrant-Create" #'+jg-tag-set-new-tag-re-entrant
                                     "Create"            #'+jg-tag-set-new-tag)

          :filtered-candidate-transformer (lambda (_c _s) (list helm-pattern)))
        )
)
;;-- end setup