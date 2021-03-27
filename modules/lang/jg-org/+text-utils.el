;;; lang/jg-org/+text-utils.el -*- lexical-binding: t; -*-
;; Functions to operate on Org Text

(defun +jg-org-insert-heading-trio ()
  (interactive)
  (org-insert-subheading 1)
  (insert "1: ")
  (org-insert-heading 3 nil nil)
  (insert "2: ")
  (org-insert-heading 1 nil nil)
  (insert "3: ")
  )
(defun +jg-org-change_link_name (name)
  """ Change the name of a link """
  (interactive "s")
  (let ((re org-link-bracket-re))
    (save-excursion
      (beginning-of-line)
      (search-forward-regexp re (line-end-position))
      (replace-match name nil nil nil 2)
      )
    )
  )

(defun +jg-org-list-agenda-files ()
  """ Creates a temporary, Org-mode buffer with links to agenda files """
  (interactive)
  (with-output-to-temp-buffer "*Agenda Files*"
    (set-buffer "*Agenda Files*")
    (insert "Agenda Files: ")
    (insert "\n")
    (mapc (lambda (x)
            (let ((file_name (last (split-string x "/" t ".org"))))
              (insert (format "[[%s][%s]]\n" x file_name))
              )) org-agenda-files)
    (org-mode)
    )
  )

(defun +jg-org-split-on-headings ()
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
    (-each (-zip-fill target-dir results '()) '+jg-text-org-split-temp-buffer-create)
    )
  )
(defun +jg-org-split-alphabetically ()
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
(defun +jg-org-split-tag-list ()
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

(defun +jg-org-wrap-numbers (a b)
  "Find numbers and wrap them in parentheses to avoid org treating them as lists"
  (interactive "r")
  (message "%s %s" a b )
  (goto-char a)
  (while (re-search-forward "^[[:space:]]*\\([[:digit:]]+\\)[.)] " b t)
    (replace-match "\n(\\1)")
    )
  )
(defun +jg-org-wrap-non-link-urls ()
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

(defun +jg-org-remove-duplicate-tweet-entries ()
  "Find duplicate tweets in an org file and remove them"
  (interactive)
  (let ((permalinks (make-hash-table :test 'equal))
        ;;Get all entries' (permalink start-bound end-bound level)
        (all-entries (seq-filter 'identity (org-map-entries '+jg-org-map-entries-build-permalink-regions)))
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
                              (plist-get (cadr (org-element-at-point)) :end)))
                       )
                  (message "Getting: %s" tweet)
                  ;; copy tweet into deletion buffer
                  (princ (buffer-substring-no-properties begin (- end 1))
                         archive-buffer)
                  ;; replace it in the original with a replaced link to the remaining
                  (delete-region begin (- end 1))
                  (goto-char begin)
                  ;; TODO Make this a link:
                  (insert (format "%s [[#%s][Duplicate of %s]]\n\n"
                                  (make-string (nth 2 tweet) ?*)
                                  (nth 0 tweet)
                                  (nth 0 tweet)))
                  )
             )
    )
  )
(defun +jg-org-map-entries-build-permalink-regions()
  " To be run with org-map-entries, extracts permalinks and regions ready to
  remove duplicates"
  (let* ((entry-details (cadr (org-element-at-point)))
         (permalink (plist-get entry-details :PERMALINK))
         (begin (plist-get entry-details :begin))
         (level (plist-get entry-details :level))
         permalink-id
         )
    (if (and permalink begin level (string-match "\\[\\[.+?\\]\\[\\(.+?\\)\\]\\]" permalink))
        (progn (setq permalink-id (match-string 1 permalink))
               (org-set-property "CUSTOM_ID" permalink-id)
               `(,permalink-id ,begin ,level))
      nil)
    )
  )
