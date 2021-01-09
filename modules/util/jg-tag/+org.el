;; org cleaning

(defun jg-tag-fill-paragraph-step-back()
  (interactive)
  (goto-char (point-max))
  (let ((prog (lambda ()
                (let ((elem (cadr (org-element-at-point))))
                  (fill-region-as-paragraph
                   (cl-getf elem :begin) (cl-getf elem :end)))))
        curr-elem
        return-to
        )
    (while (not (eq (point) (point-min)))
      (setq curr-elem (org-element-at-point)
            return-to (cl-getf (cadr curr-elem) :begin))
      (cond ((eq (car curr-elem) 'property-drawer)
             (goto-char (- (cl-getf (cadr curr-elem) :begin) 1))
             )
            ((eq (car curr-elem) 'headline)
             (goto-char (- (cl-getf (cadr curr-elem) :begin) 1))
             )
            (t
             (goto-char (cl-getf (cadr curr-elem) :begin))
             (funcall prog)
             (insert "\n")
             )
            )
      (goto-char (- return-to 1))
      )
    )
  )
(defun jg-tag-dired-clean-orgs (name)
  "A Wrapper around clean org to back up the original file "
  (message "----------")
  (message "Cleaning: %s" name)
  (with-temp-buffer
    (insert-file-contents name t)
    (write-file (format "%s_orig" name))
    (org-mode)
    (jg-tag-clean-org t)
    (write-file name)
    )
  (message "Finished Cleaning")
  )
(defun jg-tag-clean-marked-files ()
  "Clean marked Org files"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (seq-each 'jg-tag-dired-clean-orgs files)
    )
  )
(defun jg-tag-clean-org (&optional skipfck)
  "The Main Clean-org routine:
Wrap Lines
Indent
Fill-paragraphs end -> beginning
Cleanup Whitespace
Wrap twitter picture urls
Re-clean whitespace
Tidy Link positions
Indent + Whitespace + new lines

Optionally remove duplicates of links
"
  (interactive)
  (message "Starting Org Clean")
  (message "Hiding Properties")
  ;; indent region
  ;;wrap lines that are too long
  (goto-char (point-min))
  (jg-tag-wrap-non-link-urls)
  (indent-region (point-min) (point-max))
  (jg-tag-fill-paragraph-step-back)
  (whitespace-cleanup)
  (org-show-all)
  ;;Find all pic.twitter's and ensure on new line
  (goto-char (point-min))
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
  (setq jg-tag-org-clean-marker (make-marker))
  (org-map-entries 'jg-tag-map-entries-clean-whitespace t nil)
  (set-marker jg-tag-org-clean-marker nil)

  ;; Tidy all links:
  ;; DO NOT USE ORG-NEXT-LINK
  ;; it ignores links in property drawers
  (goto-char (point-min))
  (while (re-search-forward org-link-any-re nil t)
    (set-marker jg-tag-org-clean-marker (point))
    (goto-char (car (match-data 0)))
    (let ((prev-line (buffer-substring (line-beginning-position 0)
                                       (line-end-position 0))))
      (cond  ((eq 0 (string-match "^[[:space:]]+:PERMALINK:" prev-line))
              (join-line))
             ((eq 0 (string-match "^[[:space:]]+:PROPERTIES:" prev-line))
              nil)
             ((eq 0 (string-match "^[[:space:]]+:URL: " (buffer-substring (line-beginning-position) (point))))
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
    (goto-char jg-tag-org-clean-marker)
    )

  (jg-tag-org-clean-property-blocks)
  (message "Indenting")
  (indent-region (point-min) (point-max))
  (whitespace-cleanup)
  (setq jg-tag-org-clean-marker nil)

  ;;Find and replace
  (goto-char (point-min))
  (while (re-search-forward "]\\[\n[[:space:]]+" nil t)
    (replace-match "][")
    )

  (if (not skipfck)
      (progn
        (goto-char (point-min))
        (while (re-search-forward  "file:.+?%.+$" nil t)
          (if (+jg-org-link-not-exists-p)
              (progn
                (goto-char (line-beginning-position))
                (insert "--->")
                (if (s-equals? (read-string "Delete line? ") "y")
                    (delete-region (line-beginning-position) (line-end-position))
                  (progn
                    (delete-region (line-beginning-position)
                                   (+ (line-beginning-position) (length "--->")))
                    (forward-line))
                  ))
            )
          )
        )
    )

  (org-cycle-hide-drawers 'all)
  (goto-char (point-min))
  (message "Org Clean Finished")
  )
(defun jg-tag-map-entries-clean-whitespace ()

  "Called from org-map-entries. reduces whitespace prior
to point to a single new line"
  (set-marker jg-tag-org-clean-marker (line-end-position))
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
        (setq org-map-continue-from jg-tag-org-clean-marker)
        )
    )
  )
(defun jg-tag-wrap-numbers (a b)
  "Find numbers and wrap them in parentheses to avoid org treating them as lists"
  (interactive "r")
  (message "%s %s" a b )
  (goto-char a)
  (while (re-search-forward "^[[:space:]]*\\([[:digit:]]+\\)[.)] " b t)
    (replace-match "\n(\\1)")
    )
  )
(defun jg-tag-wrap-non-link-urls ()
  "Find urls that are not wrapped into org link format, and wrap them"
  (interactive)
  (let ((start (if (eq evil-state 'visual) evil-visual-beginning (point-min)))
        (last (if (eq evil-state 'visual) evil-visual-end  nil)))
    (goto-char start)
    (while (re-search-forward "[^[[]\\(http[^ …\n]+\\)" last t)
      (replace-match "[[\\1][ᵢ]]")
      )
    )
  )
(defun jg-tag-build-permalink-regions()
  " To be run with org-map-entries, extracts permalinks and regions ready to remove duplicates"
  (let* ((entry-details (cadr (org-element-at-point)))
         (permalink (plist-get entry-details :PERMALINK))
         (begin (plist-get entry-details :begin))
         (level (plist-get entry-details :level))
         )
    (if (and permalink begin level (string-match "\\[\\[.+?\\]\\[\\(.+?\\)\\]\\]" permalink))
        `(,(match-string 1 permalink) ,begin ,level)
      nil)
    )
  )
(defun jg-tag-remove-duplicates ()
  "Find duplicate tweets in an org file and remove them"
  (interactive)
  (let ((permalinks (make-hash-table :test 'equal))
        ;;Get all entries' (permalink start-bound end-bound level)
        (all-entries (seq-filter 'identity (org-map-entries 'jg-tag-build-permalink-regions)))
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
             do (let* ((begin (nth 1 tweet))
                       (end (progn
                              (goto-char begin)
                              (plist-get (cadr (org-element-at-point)) :end))))
                  (message "Getting: %s" tweet)
                  ;; copy tweet into deletion buffer
                  (princ (buffer-substring-no-properties begin (- end 1))
                         archive-buffer)
                  ;; replace it in the original with a replaced link to the remaining
                  (delete-region begin (- end 1))
                  (goto-char begin)
                  (insert (format "%s Duplicate of %s\n\n"
                                  (make-string (nth 2 tweet) ?*)
                                  (nth 0 tweet)))
                  )
             )
    )
  )

(defun jg-tag-org-format-temp-buffer (name source_name)
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
(defun jg-tag-org-split-temp-buffer-create (args)
  "Given a pair, create a temp buffer in the cdr directory,
naming the directory based on the first line of text and insert the car "
  ;; (message "Creating Temp buffer for: %s" args)
  (assert (f-dir? (cdr args)))
  (with-temp-buffer
    (org-mode)
    (insert (car args))
    (goto-char (point-min))
    (re-search-forward "^\\*\\* ")
    (write-file (f-join (cdr args) (format "%s.org" (string-trim (buffer-substring (point) (line-end-position))))))
    )
  )
(defun jg-tag-org-split-on-headings ()
  " Split an org file into multiple smaller buffers non-destructively "
  (interactive)
  (let ((contents (buffer-substring (point-min) (point-max)))
        (target-depth (read-number "What Depth Subtrees to Copy? "))
        (target-dir (read-directory-name "Split into directory: "))
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
      )
    (-each (-zip-fill target-dir results '()) 'jg-tag-org-split-temp-buffer-create)
    )
  )
(defun jg-tag-org-split-alphabetically ()
  " Split a buffer of values on separate lines into headings alphabetically "
  (interactive)
  (goto-char (point-min))
  (let ((current ?a)
        (end ?z))
    (insert "* Top\n")
    (while (and (<= current end)
                (re-search-forward (format "^%c" current)))
      (goto-char (line-beginning-position))
      (insert (format "** %c\n" current))
      (cl-incf current)
      )
    )
  )
(defun jg-tag-org-split-tag-list ()
  " Combine the org-split functions into a single routine.
Sort, align, split, save "
  (interactive)
  (let ((text (buffer-string))
        (sort-fold-case t))
    (with-temp-buffer
      (insert text)
      (sort-lines nil (point-min) (point-max))
      (align-regexp (point-min) (point-max) "\\(\\s-*\\):" 1 nil t)
      (jg-tag-org-split-alphabetically)
      (jg-tag-org-split-on-headings)
      )
    )
  )

(defun jg-tag-org-clean-property-blocks ()
  (goto-char (point-min))
  ;; Find properties block
  (let ((start-marker (make-marker))
        (end-marker (make-marker)))
  (while (search-forward-regexp ":PROPERTIES:" nil t)
    (set-marker start-marker (point))
    (search-forward ":END:" nil t)
    (goto-char (nth 0 (match-data)))
    (set-marker end-marker (point))
    (goto-char start-marker)
    (while (< (point) end-marker)
      (join-line 1)
      )
    (goto-char start-marker)
    ;; Loop over adding new lines appropriately
    (while (and (< (point) end-marker) (search-forward-regexp "\\(:[_A-Z]+:\\)" nil t))
      (replace-match "\n\\&")
      )
    )
  )
  )

(after! (evil org helm)
  (jg-tag-rebuild-tag-database)

  (evil-define-operator jg-tag-helm-start (beg end rest)
    (interactive "<R>")
    (jg-tag-helm-tagger beg end)
    )
  )
