;; bibtex
(defun tag-unify/build-bibtex-list ()
  "Build a list of all bibtex files to use for bibtex-helm "
  (mapcar (lambda (x) (f-join tag-unify/loc-bibtex x))
          (-filter (lambda (x) (s-equals? (f-ext x) "bib"))
                   (directory-files tag-unify/loc-bibtex))))
(defun tag-unify/bibtex-set-tags (x)
  " Set tags in bibtex entries "
  (let* ((visual-candidates (helm-marked-candidates))
         (actual-candidates (mapcar (lambda (x) (cadr (assoc x tag-unify/tag-unify-candidates-names))) visual-candidates))
         (prior-point 1)
         (end-line (cdr tag-unify/tag-unify-region))
         (current-tags '())
         (add-func (lambda (candidate)
                     (if (not (-contains? current-tags candidate))
                         (progn
                           (push candidate current-tags)
                           (puthash candidate 1 tag-unify/global-tags))
                       (progn
                         (setq current-tags (remove candidate current-tags))
                         (puthash candidate (- (gethash candidate tag-unify/global-tags) 1) tag-unify/global-tags))
                       )))
         )
    (save-excursion
      (goto-char (car tag-unify/tag-unify-region))
      (setq prior-point (- (point) 1))
      (while (and (/= prior-point (point)) (< (line-number-at-pos (point)) end-line))
        (progn (setq current-tags (split-string (bibtex-autokey-get-field "tags") "," t " ")
                     prior-point (point))
               (mapc add-func actual-candidates)
               (bibtex-set-field "tags" (string-join current-tags ","))
               (org-ref-bibtex-next-entry)
               )))
    )
  )
(defun tag-unify/bibtex-set-new-tag (x)
  "A Fallback function to set tags of bibtex entries "
  (save-excursion
    (goto-char (car tag-unify/tag-unify-region))
    (let ((prior-point (- (point) 1))
          (end-line (cdr tag-unify/tag-unify-region))
          (stripped_tag (tag-unify/strip_spaces x))
          )
      (while (and (/= prior-point (point)) (< (line-number-at-pos (point)) end-line))
        (setq prior-point (point))
        (let* ((current-tags (split-string (bibtex-autokey-get-field "tags") "," t " ")))
          (if (not (-contains? current-tags stripped_tag))
              (progn
                (push stripped_tag current-tags)
                (puthash stripped_tag 1 tag-unify/global-tags)))
          (bibtex-set-field "tags" (string-join current-tags ","))
          (org-ref-bibtex-next-entry)
          ))))
  )
(defun tag-unify/unify-pdf-locations-in-file (name)
  "Change all pdf locations in bibtex file to relative,
ensuring they work across machines "
  (message "Unifying Locations in %s" name)
  (with-temp-buffer
    (insert-file-contents name t)
    (goto-char (point-min))
    (while (re-search-forward "file[[:digit:]]* ?= *{\\(.+mega\\)/\\(mendeley\\)?" nil t)
      (replace-match "~/Mega" nil nil nil 1)
      (if (eq 6 (length (match-data)))
          (replace-match "pdflibrary" t nil nil 2))
      )
    (write-file name)
    )
  )
(defun tag-unify/unify-pdf-locations ()
  "Dired function to unify file locations to relative"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (seq-each 'tag-unify/unify-pdf-locations-in-file files)
    )
  )

;; file name processing
(defun tag-unify/chop-long-file (name &optional preferred-length)
  "Take long org files and split them into multiple files
If preferred-length is not specified, use tag-unify/preferred-linecount-for-org
"
  (message "----------")
  (message "Chopping: %s" name)
  (with-temp-buffer
    (insert-file-contents name t)
    (goto-char (point-min))
    (let* ((count 1)
           (base_name (file-name-sans-extension name))
           (internal_name (buffer-substring (+ 2 (point-min)) (line-end-position)))
           (master_name (format "%s_master.org" base_name))
           (regexp "^\\*\\*[^*]")
           (last-position (re-search-forward regexp nil t))
           (linecount 0)
           (fn-fn (lambda () (format "%s_%s.org" base_name count)))
           (ln-fn (lambda (a b) (- (line-number-at-pos (max a b))
                                   (line-number-at-pos (min a b)))))
           )
      (append-to-file (format "* %s\n" internal_name) nil master_name)
      (while (re-search-forward "^\\*\\*[^*]" nil t )
        (if (not (file-exists-p (funcall fn-fn)))
            (progn (message "Creating %s" (funcall fn-fn))
                   (append-to-file (format "* %s %s\n" internal_name count) nil (funcall fn-fn))
                   (append-to-file (format "** [[%s][%s %s]]\n" (funcall fn-fn) internal_name count) nil master_name)
                   )
          )
        (append-to-file "\n** " nil (funcall fn-fn))
        (append-to-file last-position (line-beginning-position) (funcall fn-fn))
        (setq linecount (+ linecount (funcall ln-fn (point) last-position))
              last-position (point))
        (if (> linecount (or preferred-length tag-unify/preferred-linecount-for-org))
            (setq linecount 0
                  count (+ 1 count))
          )
        )
      (append-to-file "\n** " nil (funcall fn-fn))
      (append-to-file last-position (point-max) (funcall fn-fn))
      )
    )
  )
(defun tag-unify/chop-long-files-from-dired ()
  "Dired action to chop each marked file"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (seq-each 'tag-unify/chop-long-file files)
    )
  )

;; org cleaning
(defun tag-unify/dired-clean-orgs (name)
  "A Wrapper around clean org to back up the original file "
  (message "----------")
  (message "Cleaning: %s" name)
  (with-temp-buffer
    (insert-file-contents name t)
    (write-file (format "%s_orig" name))
    (org-mode)
    (tag-unify/clean-org)
    (write-file name)
    )
  (message "Finished Cleaning")
  )
(defun tag-unify/clean-marked-files ()
  "A Dired buffer function to clean org files "
  (interactive)
  (let ((files (dired-get-marked-files)))
    (seq-each 'tag-unify/dired-clean-orgs)
    )
  )
(defun tag-unify/clean-org ()
  "The Main Clean-org routine"
  (interactive)
  (message "Starting Org Clean")

  (message "Hiding Properties")
  ;; indent region
  (spacemacs/indent-region-or-buffer)
  (whitespace-cleanup)
  ;; fill
  (fill-region (point-min) (point-max))

  ;;Reset to beginning
  (goto-char (point-min))
  ;;Find all pic.twitter's and ensure on new line
  (message "Finding pic.twitter's")
  (while (search-forward "pic.twitter" nil t)
    (let ((sub (buffer-substring (line-beginning-position) (point))))
      (if (not (eq 0 (string-match "^[[:space:]]+pic.twitter" sub)))
          (progn
            (backward-char (+ 1 (length "pic.twitter")))
            (insert "\n\n")))
      (progn (while (eq 0 (string-match "^[[:space:]]*$" (buffer-substring (line-beginning-position -0)
                                                                           (line-end-position -0))))
               (join-line)
               )
             (goto-char (line-beginning-position))
             (insert "\n")
             (forward-line))
      )
    )
  ;; Clean Whitespace
  (message "Cleaning Whitespace")
  (setq tag-unify/org-clean-marker (make-marker))
  (org-map-entries 'tag-unify/map-entries-clean-whitespace t nil)
  (set-marker tag-unify/org-clean-marker nil)

  (goto-char (point-min))
  ;; DO NOT USE ORG-NEXT-LINK
  ;; it ignores links in property drawers
  ;; (debug)
  (while (re-search-forward org-link-any-re nil t)
    (set-marker tag-unify/org-clean-marker (point))
    (goto-char (car (match-data 0)))
    (let ((prev-line (buffer-substring (line-beginning-position 0)
                                       (line-end-position 0))))
      (cond  ((eq 0 (string-match "^[[:space:]]+:PERMALINK:" prev-line))
              (join-line))
             ((eq 0 (string-match "^[[:space:]]+:PROPERTIES:" prev-line))
              nil)
             ((not (eq 0 (string-match "^[[:space:]]*$" (buffer-substring
                                                         (line-beginning-position)
                                                         (point)))))
              (insert "\n")
              )
             (t
              (while (eq 0 (string-match "^[[:space:]]*$" (buffer-substring
                                                           (line-beginning-position 0)
                                                           (line-end-position 0))))
                (join-line)
                )
              )
             )
      )
    (goto-char tag-unify/org-clean-marker)
    )
  (message "Indenting")
  (spacemacs/indent-region-or-buffer)
  (whitespace-cleanup)
  (setq tag-unify/org-clean-marker nil)

  (goto-char (point-min))
  (while (re-search-forward "]\\[\n[[:space:]]+" nil t)
    (replace-match "][")
    )
  (org-cycle-hide-drawers 'all)
  (message "Org Clean Finished")
  (goto-char (point-min ))
  )
(defun tag-unify/map-entries-clean-whitespace ()
  "Called from org-map-entries. reduces whitespace prior
to point to a single new line"
  (set-marker tag-unify/org-clean-marker (line-end-position))
  (if (not (eq (point) (point-min)))
      (progn
        (while (eq 0 (string-match "^[[:space:]]*$"
                                   (buffer-substring
                                    (line-beginning-position 0)
                                    (line-end-position 0))))
          (join-line))
        (if (not (string-equal "*" (buffer-substring
                                    (line-beginning-position 0)
                                    (+ 1 (line-beginning-position 0)))))
            (insert "\n"))
        (setq org-map-continue-from tag-unify/org-clean-marker)
        )
    )
  )
(defun tag-unify/wrap-numbers (a b)
  "Find numbers and wrap them in parentheses to avoid org treating them as lists"
  (interactive "r")
  (message "%s %s" a b )
  (goto-char a)
  (while (re-search-forward "^[[:space:]]*\\([[:digit:]]+\\)[.)] " b t)
    (replace-match "\n(\\1)")
    )
  )
(defun tag-unify/wrap-non-link-urls ()
  "Find urls that are not wrapped into org link format, and wrap them"
  (interactive)
  (let ((start (if (eq evil-state 'visual) evil-visual-beginning (point-min)))
        (last (if (eq evil-state 'visual) evil-visual-end  nil)))
    (goto-char start)
    (while (re-search-forward "[^[[]\\(http[^ …]+\\)" last t)
      (replace-match "[[\\1][ᵢ]]")
      )
    )
  )
(defun tag-unify/build-permalink-regions()
  " To be run with org-map-entries, extracts permalinks and regions ready to remove duplicates"
  (let* ((entry-details (cadr (org-element-at-point)))
         (permalink (plist-get entry-details :PERMALINK))
         (begin (plist-get entry-details :begin))
         (end (plist-get entry-details :end))
         (level (plist-get entry-details :level))
         )
    (if (and permalink begin end level (string-match "\\[\\[.+?\\]\\[\\(.+?\\)\\]\\]" permalink))
        `(,(match-string 1 permalink) ,begin ,end ,level)
      nil)
    )
  )
(defun tag-unify/remove-duplicates ()
  "Find duplicate tweets in an org file and remove them"
  (interactive)
  (let ((permalinks (make-hash-table :test 'equal))
        ;;Get all entries' (permalink start-bound end-bound level)
        (all-entries (seq-filter 'identity (org-map-entries 'tag-unify/build-permalink-regions)))
        (to-remove '())
        (archive-buffer (get-buffer-create "*Org-Cleanup-Tweets*"))
        )
    ;;Process all-entries, storing the first instance of a tweet in the hashmap,
    ;;all subsequent instances in the to-remove list
    (cl-loop for tweet in all-entries
             do (if (not (gethash (nth 0 tweet) permalinks))
                    (let ((m (make-marker)))
                      (move-marker m (nth 1 tweet))
                      (puthash (nth 0 tweet) m  permalinks))
                  (push tweet to-remove)
                  )
             )
    (message "To Remove: %s" (length to-remove))
    ;;Now remove those duplicates
    (cl-loop for tweet in to-remove
             do (progn
                  (message "Getting: %s" tweet)
                  ;; copy tweet into deletion buffer
                  (princ (buffer-substring-no-properties (nth 1 tweet)
                                                         (- (nth 2 tweet) 1))
                         archive-buffer)
                  ;; replace it in the original with a replaced link to the remaining
                  (delete-region (nth 1 tweet) (- (nth 2 tweet) 1))
                  (goto-char (nth 1 tweet))
                  (insert (format "%s Duplicate of %s\n\n"
                                  (make-string (nth 3 tweet) ?*)
                                  (nth 0 tweet)))
                  )
             )
    )
  )

;; helm
(defun tag-unify/open-url-action (x)
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
        (jg_org/open_link_externally)
        (forward-line)
        )
      )
    )
  )
(defun tag-unify/insert-candidates (x)
  "A Helm action to insert selected candidates into the current buffer "
  (let ((candidates (helm-marked-candidates)))
    (with-helm-current-buffer
      ;;Substring -2 to chop off separating marks
      (insert (mapconcat (lambda (x) (substring x 0 -2)) candidates "\n")))))
(defun tag-unify/insert-links (x)
  "Helm action to insert selected candidates formatted as org links"
  (let ((candidates (helm-marked-candidates)))
    (with-helm-current-buffer
      ;;substring -2 to chop off separating marks
      (insert (mapconcat (lambda (x) (format "[[%s][%s]]" (substring x 0 -2) (substring x 0 -2))) candidates "\n")))))
(defun tag-unify/tweet-link-action (candidate)
  "Helm action to open a tweet buffer with the link inserted"
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
  "Action to finish and tweet a link"
  (interactive)
  (let* ((text (buffer-substring-no-properties (point-min) (point-max))))
    (jg_twitter/twitter-tweet-text text nil '(jg_twitter/tweet_sentinel))
    ))
(defun tag-unify/grep-filter-one-by-one (candidate)
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
(defun tag-unify/org-set-tags (x)
  """ Improved action to add and remove tags Toggle Selected Tags
Can operate on regions of headings """
  (let* ((visual-candidates (helm-marked-candidates))
         (actual-candidates (mapcar (lambda (x) (cadr (assoc x tag-unify/tag-unify-candidates-names))) visual-candidates))
         (prior-point 1)
         (end-line (cdr tag-unify/tag-unify-region))
         (current-tags '())
         (add-func (lambda (candidate)
                     (if (not (-contains? current-tags candidate))
                         (progn
                           (push candidate current-tags)
                           (puthash candidate 1 tag-unify/global-tags))
                       (progn
                         (setq current-tags (remove candidate current-tags))
                         (puthash candidate (- (gethash candidate tag-unify/global-tags) 1) tag-unify/global-tags))
                       ))))
    (save-excursion
      (goto-char (car tag-unify/tag-unify-region))
      (setq prior-point (- (point) 1))
      (while (and (/= prior-point (point)) (<= (line-number-at-pos (point)) end-line))
        (progn (setq current-tags (org-get-tags nil t)
                     prior-point (point))
               (mapc add-func actual-candidates)
               (org-set-tags current-tags)
               (org-forward-heading-same-level 1)
               )))))
(defun tag-unify/find-file (x)
  "A simple helm action to open selected files"
  (let ((files (if (helm-marked-candidates) (helm-marked-candidates) (list x))))
    (mapc 'find-file (mapcar 'string-trim files))
    )
  )

;; tags
(defun tag-unify/get-buffer-tags (&optional name depth)
  "Process a buffer and get all tags from a specified depth of heading
if no depth is specified, get all tags of all headings
returns a hash-table of the tags, and their instance counts.
"
  (let ((tag-set (make-hash-table :test 'equal))
        (tagdepth-p (if (and depth (> depth 0)) (lambda (x) (eq depth x)) 'identity))
        )
    ;; (message "Get buffer tags: %s %s" name tagdepth-p)
    (with-current-buffer (if name name (current-buffer))
      (save-excursion ;;store where you are in the current
        (goto-char (point-min))
        ;;where to store tags:
        ;;split tags into list
        (mapc (lambda (x) (incf (gethash x tag-set 0)))
              ;;TODO: fix tag depth filtering
              (-flatten
               (org-map-entries (lambda () (if (funcall tagdepth-p (car (org-heading-components)))
                                               (org-get-tags nil t) '())))))
        tag-set
        )
      )
    )
  )
(defun tag-unify/get-file-tags (filename &optional depth)
  "Get tags from a specified file, at an org specified depth.
If depth is not specified, default to get all tags from all headings
Return a hash-table of tags with their instance counts"
  (let ((tagcounts (make-hash-table :test 'equal))
        (tagdepth-p (if (and depth (> depth 0)) (lambda (x) (eq depth x)) 'identity))
        raw-tags
        )
    ;; (message "Get file tags: %s %s" filename depth)
    (with-temp-buffer
      (insert-file filename)
      (org-mode)
      (goto-char (point-min))
      (setq raw-tags (org-map-entries (lambda () (if (funcall tagdepth-p (car (org-heading-components))) (org-get-tags nil t) '()))))
      )
    (mapc (lambda (x) (incf (gethash x tagcounts 0))) (-flatten raw-tags))
    tagcounts
    )
  )

(defun tag-unify/tag-occurrences ()
  " Create a Bar Chart of Tags in the current buffer "
  (interactive)
  (let* ((depth-arg evil-ex-argument)
         (depth (if depth-arg (string-to-number depth-arg) nil))
         (alltags (make-hash-table :test 'equal))
         )
    (if (eq 'org-mode major-mode)
        (progn
          ;; (message "Getting Tags for all buffers to depth: %s" depth)
          (maphash (lambda (k v) (incf (gethash k alltags 0) v)) (tag-unify/get-buffer-tags nil depth))
          (if (not (hash-table-empty-p alltags))
              (tag-unify/chart-tag-counts alltags (buffer-name))
            (message "No Tags in buffer")))
      (message "Not in an org buffer")
      )
    )
  )
(defun tag-unify/tag-occurrences-in-open-buffers()
  """ Retrieve all tags in all open buffers, print to a temporary buffer """
  (interactive "p")
  (let* ((allbuffers (buffer-list))
         (alltags (make-hash-table :test 'equal))
         (depth (if depth-arg (string-to-number depth-arg) nil))
         )
    ;; (message "Getting Tags for all buffers to depth: %s" depth)
    (loop for x in allbuffers do
          (if (with-current-buffer x (eq 'org-mode major-mode))
              (maphash (lambda (k v) (if (not (gethash k alltags)) (puthash k 0 alltags))
                         (incf (gethash k alltags) v)) (tag-unify/get-buffer-tags x depth))
            )
          )
    (if (not (hash-table-empty-p alltags))
        (tag-unify/chart-tag-counts alltags "Active Files")
      (message "No Tags in buffers"))
    )
  )
(defun tag-unify/describe-marked-tags ()
  "Dired action to describe tags in marked files"
  (interactive)
  (let ((marked (dired-get-marked-files))
        (targetdepth (or current-prefix-arg 2))
        (alltags (make-hash-table :test 'equal))
        )
    ;; (message "Describing marked file tags to depth: %s" targetdepth)
    (loop for x in marked do
          (maphash (lambda (k v) (incf (gethash k alltags 0) v)) (tag-unify/get-file-tags x targetdepth))
          )
    (if (not (hash-table-empty-p alltags))
        (tag-unify/chart-tag-counts alltags "Dired Marked Files")
      (message "No Tags in Files")
      )
    )
  )

(defun tag-unify/mark-untagged-orgs ()
  "Dired action to mark org files which are not tagged at heading depth 2"
  (interactive)
  (dired-map-over-marks
   (progn (if (or (not (f-ext? (dired-get-filename) "org"))
                  (tag-unify/org-tagged-p (dired-get-filename)))
              (dired-unmark 1)))
   nil
   )
  )
(defun tag-unify/org-tagged-p  (filename)
  "Test an org file. Returns true if the file has tags for all depth 2 headings"
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (org-mode)
    (let* ((mapped (org-map-entries (lambda () `(,(car (org-heading-components)) ,(org-get-tags nil t)))))
           (filtered (seq-filter (lambda (x) (and (eq 2 (car x)) (null (cadr x)))) mapped)))
      (seq-empty-p filtered)
      )
    )
  )

(defun tag-unify/chart-tag-counts (counthash name)
  "Given a hashtable of counts, create a buffer with a bar chart of the counts"
  ;; (message "Charting: %s %s" counthash name)
  (let* ((hashPairs (-zip (hash-table-keys counthash) (hash-table-values counthash)))
         (sorted (sort hashPairs (lambda (a b) (> (cdr a) (cdr b)))))
         (maxTagLength (apply 'max (mapcar (lambda (x) (length (car x))) sorted)))
         (maxTagAmnt (apply 'max (mapcar (lambda (x) (cdr x)) sorted)))
         )
    ;;print them all out

    (with-temp-buffer-window "*Tags*"
                             nil
                             nil
                             ;; Todo: Expand this func to group and add org headings
                             (mapc (lambda (x) (princ (format "%s\n" x)))
                                   (tag-unify/make-bar-chart sorted maxTagLength maxTagAmnt))
                             )
    (tag-unify/org-format-temp-buffer "*Tags*" name)
    )
  )

(defun tag-unify/set-tags (x)
  "Utility action to set tags. Works in org *and* bibtex files"
  (if (eq major-mode 'bibtex-mode)
      (tag-unify/bibtex-set-tags x)
    (tag-unify/org-set-tags x))
  )
(defun tag-unify/set-new-tag (x)
  "Utility action to add a new tag. Works for org *and* bibtex"
  (if (eq major-mode 'bibtex-mode)
      (tag-unify/bibtex-set-new-tag x)
    (tag-unify/org-set-new-tag x))
  )
(defun tag-unify/org-set-new-tag (x)
  "Utility to set a new tag for an org heading"
  (save-excursion
    (goto-char (car tag-unify/tag-unify-region))
    (let ((prior-point (- (point) 1))
          (end-line (cdr tag-unify/tag-unify-region))
          (stripped_tag (tag-unify/strip_spaces x))
          )
      (while (and (/= prior-point (point)) (< (line-number-at-pos (point)) end-line))
        (setq prior-point (point))
        (let* ((current-tags (org-get-tags nil t)))
          (if (not (-contains? current-tags stripped_tag))
              (progn
                (push stripped_tag current-tags)
                (puthash stripped_tag 1 tag-unify/global-tags)))
          (org-set-tags current-tags)
          (org-forward-heading-same-level 1)
          )))))

(defun tag-unify/auto-tag-marked-files ()
  " Process marked files, adding tags to threads if
already used tag keywords are in the thread"
  (interactive)
  (assert (not (hash-table-empty-p tag-unify/global-tags)))
  (let ((marked-files (dired-get-marked-files)))
    (loop for x in marked-files do
          (tag-unify/auto-tag-file x)
          )
    )
  )
(defun tag-unify/auto-tag-this-file ()
  (interactive)
  (goto-char (point-min))
  (org-map-tree 'tag-unify/auto-tag-thread)
  )
(defun tag-unify/auto-tag-file (x)
  "Given a file, load it and process each thread"
  (with-temp-buffer
    (insert-file-contents x)
    (org-mode)
    (goto-char (point-min))
    (org-map-tree 'tag-unify/auto-tag-thread)
    )
  )
(defun tag-unify/auto-tag-thread ()
  "Called on each heading of a file, only run
for headings of depth 2
Use all words in thread as queries to master tag list,
add matches to thread tags
"
  (message "Auto Tagging thread: %s" (org-get-heading))
  (let ((depth (car (org-heading-components)))
        (to-tag-list (make-hash-table :test 'equal))
        (curr-tags (org-get-tags nil t))
        words)
    (if (eq depth 2)
        (progn
          (setq words (s-split-words (substring-no-properties (org-get-entry))))
          (loop for x in words do
                (if (gethash x tag-unify/global-tags)
                    (puthash x 1 to-tag-list)
                  )
                )
          (if to-tag-list
              (org-set-tags (union (hash-table-keys to-tag-list) curr-tags :test 'equal))
            )
          )
      )
    )
  )

;; utility
(defun tag-unify/process-candidates (x)
  "Utility to tidy bibtex-completion-candidates for helm-bibtex"
  (cons (s-replace-regexp ",? +" " " (car x))
        (cdr x))
  )
(defun tag-unify/rebuild-tag-database ()
  "Rebuild the tag database from global-tags-location "
  (interactive)
  (clrhash tag-unify/global-tags)
  (if (f-exists? tag-unify/global-tags-location)
      (with-temp-buffer
        (insert-file tag-unify/global-tags-location)
        (goto-char (point-min))
        (while (< (point) (point-max))
          ((lambda (x) (puthash (car x) (string-to-number (cadr x)) tag-unify/global-tags)) (split-string (buffer-substring (line-beginning-position) (line-end-position)) ":" nil " "))
          (forward-line)
          )
        )
    (message "ERROR: GLOBAL-TAGS-LOCATION IS EMPTY")
    )
  )
(defun tag-unify/strip_spaces (str)
  "Utility to replace spaces with underscores in a string.
Used to guard inputs in tag strings"
  (s-replace " " "_" (string-trim str))
  )
(defun tag-unify/sort-candidates (ap bp)
  " Sort routine to sort by colour then lexicographically "
  (let* ((a (car ap))
         (b (car bp))
         (aprop (get-text-property 0 'font-lock-face a))
         (bprop (get-text-property 0 'font-lock-face b))
         (lookup (lambda (x) (gethash (cadr x) tag-unify/global-tags))))
    (cond
     ((and aprop bprop (> (funcall lookup ap) (funcall lookup bp))) t)
     ((and aprop (not bprop)) t)
     ((and (not aprop) (not bprop) (> (funcall lookup ap) (funcall lookup bp))))
     )))
(defun tag-unify/tag-unify-candidates ()
  " Given Candidates, colour them if they are assigned, then sort them  "
  (let* ((buffer-cand-tags (tag-unify/get-buffer-tags))
         (global-tags tag-unify/global-tags))
    (if (not (hash-table-empty-p global-tags))
        (let* ((cand-keys (hash-table-keys global-tags))
               (cand-vals (hash-table-values global-tags))
               (cand-pairs (-zip cand-keys cand-vals))
               (maxTagLength (apply 'max (mapcar 'length cand-keys)))
               (maxTagAmount (apply 'max cand-vals))
               (bar-keys (tag-unify/make-bar-chart cand-pairs maxTagLength maxTagAmount))
               (display-pairs (-zip bar-keys cand-keys))
               (current-tags (org-get-tags nil t))
               (propertied-tags (map 'list (lambda (candidate)
                                             (let ((candString (car candidate)))
                                               (if (-contains? current-tags (cdr candidate))
                                                   (progn (put-text-property 0 (length candString)
                                                                             'font-lock-face
                                                                             'rainbow-delimiters-depth-1-face
                                                                             candString)))
                                               `(,candString ,(cdr candidate)))) display-pairs))
               )
          (setq tag-unify/tag-unify-candidate-counts global-tags)
          (setq tag-unify/tag-unify-candidates-names (sort propertied-tags 'tag-unify/sort-candidates))
          )
      '()
      ))
  )
(defun tag-unify/make-bar-chart (data maxTagLength maxTagAmnt)
  " Make a bar chart from passed in hashtable and descriptive information "
  (let* ((maxTagStrLen (length (number-to-string maxTagAmnt)))
         (maxTagLength-bounded (min 40 maxTagLength))
         (max-column (- fill-column (+ 3 maxTagLength-bounded maxTagStrLen 3 3)))
         (bar-div (/ (float max-column) maxTagAmnt)))
    (mapcar 'tag-unify/bar-chart-line data)))

(defun tag-unify/bar-chart-line (x)
  "Construct a single line of a bar chart"
  (let* ((tag (car x))
         (tag-len (length tag))
         (tag-cut-len (- maxTagLength-bounded 3))
         (tag-truncated-p (> tag-len maxTagLength-bounded))
         (tag-substr (if tag-truncated-p (string-join `(,(substring tag nil tag-cut-len) "..."))
                       tag))
         (tag-final-len (length tag-substr))
         (amount (cdr x))
         (amount-str (number-to-string amount))
         (sep-offset (- (+ 3 maxTagLength-bounded) tag-final-len))
         (amount-offset (- maxTagStrLen (length amount-str)))
         (bar-len (ceiling (* bar-div amount)))
         )
    (string-join `(,tag-substr
                   ,(make-string sep-offset ?\ )
                   " : "
                   ,amount-str
                   ,(make-string amount-offset ?\ )
                   " : "
                   ,(make-string bar-len ?=)
                   ;; "\n"
                   )))
  )

(defun tag-unify/org-format-temp-buffer (name source_name)
  " Format bar chart buffer as an org buffer.
Adds a header, separates similar counted lines into sub headings,
and sorts groups alphabetically"
  (with-current-buffer name
    (org-mode)
    (let ((inhibit-read-only 't)
          (last_num nil)
          (get_num_re ": \\([[:digit:]]+\\) +:")
          (start-marker (make-marker))
          (end-marker (make-marker))
          (sort-fold-case t)
          matched
          )
      ;;Loop over all lines
      (goto-char (point-min))
      (set-marker start-marker (point))
      (while (re-search-forward get_num_re nil 't)
        (setq matched (match-string 1))
        (cond
         ((not last_num) t)
         ((not (string-equal last_num matched))
          (progn (set-marker end-marker (line-beginning-position))
                 (if (> (- end-marker 1) start-marker)
                     (sort-lines nil start-marker (- end-marker 1)))
                 (goto-char start-marker)
                 (insert "** ")
                 (goto-char end-marker)
                 (set-marker start-marker end-marker)
                 )))
        (setq last_num matched)
        (forward-line)
        )
      ;;clean up last group:
      (set-marker end-marker (line-beginning-position))
      (if (> end-marker start-marker)
          (sort-lines nil start-marker (- end-marker 1)))
      (goto-char start-marker)
      (insert "** ")
      ;;Add Header:
      (goto-char (point-min))
      (insert "* Tag Summary for: " source_name "\n")
      (indent-region (point-min) (point-max))
      )
    )
  )
(defun tag-unify/org-split-temp-buffer-create (args)
  "Given a pair, create a temp buffer based on the cdr,
and insert the car "
  ;; (message "Creating Temp buffer for: %s" args)
  (with-temp-buffer-window (make-temp-name (cdr args)) nil nil
                           (org-mode)
                           (princ (car args))))
(defun tag-unify/org-split-on-headings ()
  " Split an org file into multiple smaller buffers non-destructively "
  (interactive)
  (let ((contents (buffer-substring (point-min) (point-max)))
        (target-depth (read-number "What Depth Subtrees to Copy? "))
        (orig-name (file-name-sans-extension (buffer-name)))
        (map-fn (lambda ()
                  (let* ((components (org-heading-components))
                         (depth (car components)))
                    ;;Only copy correct depths
                    (if (eq depth target-depth)
                        (progn
                          ;; (message (format "Current : %s %s" count (nth 4 components)))
                          (org-copy-subtree 1)
                          (current-kill 0 t)
                          )
                      )
                    )
                  ))
        results
        )
    (with-temp-buffer
      (org-mode)
      (insert contents)
      (goto-char (point-min))
      (setq results (-non-nil (org-map-entries map-fn)))
      (-each (-zip-fill orig-name results '()) 'tag-unify/org-split-temp-buffer-create)
      )
    )
  )

(defun tag-unify/move-links ()
  " Go through all links in a file,
and either copy, or move, the the referenced file to a new location
Prefix-arg to move the file otherwise copy it
"
  (interactive)
  ;;Specify target, or use default
  (let ((target (read-directory-name "Move To: "
                                     "/Volumes/Overflow/missing_images/"))
        (action (if current-prefix-arg 'rename-file 'copy-file))
        link
        )
    (if (not (file-exists-p target))
        (progn (message "Making target directory: %s" target)
               (mkdir target))
      )
    (message "Process Type: %s" action)
    (goto-char (point-min))
    (while (eq t (org-next-link))
      (setq link (plist-get (plist-get (org-element-context) 'link) :path))
      (message "Processing: %s" link)
      (if (not (f-exists? (f-join target (-last-item (f-split link)))))
          (funcall action link target)
        (message "Already Exists"))
      )
    )
  )

(defun tag-unify/find-random-marked-file ()
  "Dired action to open a random file from the marked selection"
  (interactive)
  (let ((marked (dired-get-marked-files)))
    (find-file (nth (random (length marked))
                    marked))
    )
  )

;; Indexing
(defun tag-unify/index-people ()
  " Run rountine to index all twitter users in the current directory "
  (interactive)
  ;; Get all org files
  (let ((all-orgs (directory-files-recursively (dired-current-directory) "\.org"))
        (index-hash (make-hash-table :test 'equal))
        (inserted-for-file (make-hash-table :test 'equal))
        (curr-d (dired-current-directory))
        )
    (message "Found %s org files" (length all-orgs))
    ;; For each org collect people in file
    (cl-loop for filename in all-orgs
             do (with-temp-buffer
                  (clrhash inserted-for-file)
                  (insert-file filename)
                  (goto-char (point-min))
                  (while (re-search-forward "\*+ \\(@[_[:word:]]+\\)" nil t)
                    (let ((str (match-string 1)))
                      (if (null (gethash str inserted-for-file))
                          (progn (puthash str t inserted-for-file)
                                 (if (null (gethash str index-hash))
                                     (puthash str '() index-hash))
                                 ;; (message "Pushing %s : %s" str filename)
                                 (push filename (gethash str index-hash))))))))
    ;; create index
    (message "Accumulated %s accounts" (length (hash-table-keys index-hash)))
    (with-temp-buffer
      (maphash (lambda (k v)
                 (insert (format "%s "k))
                 (mapc (lambda (x)
                         (insert (format ":%s" x))) v)
                 (insert "\n")
                 ) index-hash)
      (write-file tag-unify/twitter-account-index t)
      )
    )
  (message "Finished writing file")
  )
(defun tag-unify/index-tags()
  " Run routine to index all tags in org files "
  (interactive)
  ;; Get all org files
  (let ((all-orgs (directory-files-recursively (dired-current-directory) "\.org"))
        (index-hash (make-hash-table :test 'equal))
        (inserted-for-file (make-hash-table :test 'equal))
        (curr-d (dired-current-directory))
        )
    (message "Found %s org files" (length all-orgs))
    ;; For each org collect tags in file
    (cl-loop for filename in all-orgs
             do (with-temp-buffer
                  (org-mode)
                  (clrhash inserted-for-file)
                  (insert-file filename)
                  (goto-char (point-min))
                  ;;Get tags:
                  (while (re-search-forward "^\\*\\* " nil t)
                    (let ((tags (org-get-tags nil t)))
                      (mapc (lambda (x)
                              (if (null (gethash x inserted-for-file))
                                  (progn (puthash x t inserted-for-file)
                                         (if (null (gethash x index-hash))
                                             (puthash x '() index-hash))
                                         (push filename (gethash x index-hash))
                                         )
                                )
                              )
                            tags)
                      )
                    )
                  )
             )
    ;; create index
    (message "Accumulated %s tags" (length (hash-table-keys index-hash)))
    (with-temp-buffer
      (maphash (lambda (k v)
                 (insert (format "%s "k))
                 (mapc (lambda (x)
                         (insert (format ":%s" x))) v)
                 (insert "\n")
                 ) index-hash)
      (write-file tag-unify/twitter-tag-index t)
      )
    )
  (message "Finished writing file")
  )
