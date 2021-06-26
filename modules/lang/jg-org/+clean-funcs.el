;;; +clean-funcs.el --- summary -*- lexical-binding: t -*-
;;
;; Main Function
(defun +jg-org-clean-master (&optional skipfck)
  (interactive)
  (goto-char (point-min))
  (org-show-all)
  (org-cycle-hide-drawers 'all)
  (+jg-org-remove-surplus-headings)
  (+jg-org-add-twitter-property)
  (+jg-org-remove-duplicate-tweet-entries)
  (+jg-org-clean-whole-duplicate-threads)
  (+jg-org-sort-headings)

  ;; Fill Paragraphs from bottom up
  (+jg-org-fill-paragraph-reverse)
  ;; Clean links
  (+jg-org-pic-twitter-clean)
  (+jg-org-wrap-non-link-urls)
  ;; Map over entries, removing extra space between entries
  (setq jg-org-clean-marker (make-marker))
  (org-map-entries '+jg-org-map-entries-clean-whitespace t nil)
  (set-marker jg-org-clean-marker nil)
  ;; Tidy all links:
  (+jg-org-property-drawer-clean-links)
  (+jg-org-clean-property-blocks)

  (setq jg-org-clean-marker nil)

  ;; Connect links together again
  (+jg-org-refill-links)

  ;; Deal with bad links
  (if (not skipfck)
      (+jg-org-find-bad-links)
    )

  ;; Hide Drawers
  (org-cycle-hide-drawers 'all)
  ;; Goto start
  (goto-char (point-min))
  (message "Org Clean Finished")
  )

(defun +jg-org-remove-surplus-headings ()
  " Go through a buffer, removing additional single star headings,
and the property block directly below "
  (message "Removing Surplus Top Level Headings")
  (goto-char (point-min))
  (forward-line)
  (let ((kill-whole-line t))
    (while (re-search-forward "^\* " nil t)
      ;; Delete the heading
      (beginning-of-line)
      (kill-line)
      ;; if theres a property block, delete it
      (while (looking-at-p "^[[:space:]]*:.+?:")
        (kill-line)
        )
      )
    )
  )
(defun +jg-org-add-twitter-property ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (org-set-property "TWITTER-BUFFER" "t")
    )
  )
(defun +jg-org-remove-duplicate-tweet-entries ()
  "Find duplicate tweets in an org file and remove them"
  (interactive)
  (message "Removing Duplicate Tweet Entries")
  (let ((permalinks (make-hash-table :test 'equal))
        ;;Get all entries' (permalink start-bound level is_quote)
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
                       (elem-data (progn
                                    (goto-char begin)
                                    (cadr (org-element-at-point))))
                       (end (plist-get elem-data :end))
                       (rem-perma (plist-get elem-data :CUSTOM_ID)))

                  (message "Removing: %s : %s" (nth 0 tweet) rem-perma)
                  (assert (s-equals? (nth 0 tweet) rem-perma))
                  ;; copy tweet into deletion buffer
                  (princ (buffer-substring-no-properties begin (- end 1))
                         archive-buffer)
                  ;; replace it in the original with a link to the remaining
                  (delete-region begin (- end 1))
                  (goto-char begin)
                  ;; As a Link:
                  (insert (format "%s [[#%s][Duplicate of %s]]\n\n"
                                  (make-string (nth 2 tweet) ?*)
                                  (nth 0 tweet)
                                  (nth 0 tweet)))
                  )
             )
    )
  )
(defun +jg-org-clean-whole-duplicate-threads ()
  " remove threads which are just trees of duplicate links "
  (message "Removing Duplicate-only subtrees")
  (goto-char (point-min))
  ;; map over level 2 subtrees
  ;; if every applicable heading is a duplicate link,
  ;; mark it for removal the entire subtree
  (defvar jg-dup-2-star (make-marker))
  (defvar jg-dup-3-star (make-marker))
  (defvar jg-dup-hash-log (make-hash-table))
  (clrhash jg-dup-hash-log)

  (org-map-entries '+jg-org-map-entry-duplicate-finder t nil)
  ;; remove marked subtrees
  (let* ((filtered-keys (-filter #'(lambda (x) (gethash x jg-dup-hash-log))
                                 (hash-table-keys jg-dup-hash-log)))
         (sorted-keys (sort filtered-keys #'>))
         )
    (loop for pos in sorted-keys
          do
          (goto-char pos)
          (org-cut-subtree))
    )

  (makunbound 'jg-dup-2-star)
  (makunbound 'jg-dup-3-star)
  (makunbound 'jg-dup-hash-log)
  )
(defun +jg-org-pic-twitter-clean ()
  ;; Find all pic.twitter's and ensure on new line
  (goto-char (point-min))
  (message "Finding pic.twitter's")
  (while (search-forward "pic.twitter" nil t)
    (let ((sub (buffer-substring (line-beginning-position) (point))))
      (if (not (eq 0 (string-match "^[[:space:]]+pic.twitter" sub)))
          (progn (backward-char (+ 1 (length "pic.twitter")))
                 (insert "\n\n")))
      (progn (while (eq 0 (string-match "^[[:space:]]*$"
                                        (buffer-substring (line-beginning-position -0)
                                                          (line-end-position -0))))
               (join-line))
             (goto-char (line-beginning-position))
             (insert "\n")
             (forward-line)
             (end-of-line)
             )
      )
    )
  )
(defun +jg-org-property-drawer-clean-links ()
  ;; DO NOT USE ORG-NEXT-LINK
  ;; it ignores links in property drawers
  (goto-char (point-min))
  (while (re-search-forward org-link-any-re nil t)
    (set-marker jg-org-clean-marker (point))
    (goto-char (car (match-data 0)))
    (let ((prev-line (buffer-substring (line-beginning-position 0)
                                       (line-end-position 0))))
      (cond ((eq 0 (string-match (rx line-start (0+ blank) ?:
                                     (or "PROPERTIES" "END") ?:
                                     (0+ blank) line-end) prev-line))
             nil)
            ((eq 0 (string-match (rx line-start (0+ blank)
                                     (optional (group ?: (1+ (or ?_ word)) ?:)
                                               (0+ blank))
                                     line-end) prev-line))
             (join-line))
            ((eq 0 (string-match (rx line-start (0+ blank)
                                     ?: (1+ (or ?_ word)) ?:
                                     (0+ blank)) (buffer-substring
                                     (line-beginning-position)
                                     (point))))
             nil)
            ((not (eq 0 (string-match "^\s*$" (buffer-substring
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
    (goto-char jg-org-clean-marker)
    )
  )
(defun +jg-org-clean-property-blocks ()
  " Find Property blocks and clean newlines in them "
  ;; TODO merge with +jg-org-fix-properties-drawers
  (goto-char (point-min))
  ;; Find properties block
  (let ((end-bound (point-max))
        (prop-re (rx (group ?: (1+ (or letter ?- ?_)) ?:)))
        )
    (while (search-forward-regexp ":PROPERTIES:" end-bound t)
      (setq end-bound (plist-get (cadr (org-element-at-point)) :contents-end))
      (evil-join (plist-get (cadr (org-element-at-point)) :contents-begin)
                 end-bound)
      (goto-char (line-beginning-position))
      ;; Loop over adding new lines appropriately
      (search-forward-regexp prop-re (line-end-position) t)
      (while (search-forward-regexp prop-re (line-end-position) t)
        (replace-match "\n\\&")
        )
      (setq end-bound (point-max))
      )
    )
  )
;; Utils
(defun +jg-org-map-entries-clean-whitespace ()
  "Called from org-map-entries. reduces whitespace prior
to point to a single new line"
  (set-marker jg-org-clean-marker (line-end-position))
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
        (setq org-map-continue-from jg-org-clean-marker)
        )
    )
  )
(defun +jg-org-map-entry-duplicate-finder ()
  (let ((ctx (org-element-context))
        (props (org-entry-properties)))
    (cond ((alist-get "PERMALINK" props nil nil #'s-equals?)
           (puthash (marker-position jg-dup-2-star) nil jg-dup-hash-log)
           (puthash (marker-position jg-dup-3-star) nil jg-dup-hash-log)
           )
          ((s-contains? "Conversations" (plist-get (cadr ctx) :raw-value) t)
           nil)
          ((s-contains? "Links" (plist-get (cadr ctx) :raw-value) t)
           nil)
          ((s-contains? "Media" (plist-get (cadr ctx) :raw-value) t)
           nil)
          ((s-contains? "Videos" (plist-get (cadr ctx) :raw-value) t)
           nil)
          ((eq (plist-get (cadr ctx) :level) 2)
           (move-marker jg-dup-2-star (plist-get (cadr ctx) :begin))
           (puthash (marker-position jg-dup-2-star) t jg-dup-hash-log)
           (move-marker jg-dup-3-star jg-dup-2-star)
           )
          ((eq (plist-get (cadr ctx) :level) 3)
           (move-marker jg-dup-3-star (plist-get (cadr ctx) :begin))
           (puthash (marker-position jg-dup-3-star) t jg-dup-hash-log))
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
         (is-quote (plist-get entry-details :IS_QUOTE))
         permalink-id
         )
    (if (and permalink begin level (string-match "\\[\\[.+?\\]\\[\\(.+?\\)\\]\\]" permalink))
        (progn (setq permalink-id (match-string 1 permalink))
               (org-set-property "CUSTOM_ID" permalink-id)
               `(,permalink-id ,begin ,level ,is-quote))
      nil)
    )
  )

(defun +jg-org-find-bad-links ()
  "Utility function to check files exist,
asks user to delete line if file does not."
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
