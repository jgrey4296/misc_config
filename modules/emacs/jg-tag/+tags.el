;; tags
(defun +jg-tag-add-mode-handler (mode func-set func-new)
  " Register a mode symbol with a function/symbol to
call when the evil-ex command 't[ag]' is called,
or +jg-tag-set-tags +jg-tag-set-new-tag"
  (puthash mode `(,func-set . ,func-new) jg-tag-alt-mapping)
  )

(defun +jg-tag-get-buffer-tags (&optional name depth)
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
        (mapc (lambda (x) (cl-incf (gethash x tag-set 0)))
              ;;TODO: fix tag depth filtering
              (-flatten
               (org-map-entries (lambda () (if (funcall tagdepth-p (car (org-heading-components)))
                                               (org-get-tags nil t) '())))))
        tag-set
        )
      )
    )
  )
(defun +jg-tag-get-file-tags (filename &optional depth)
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
    (mapc (lambda (x) (cl-incf (gethash x tagcounts 0))) (-flatten raw-tags))
    tagcounts
    )
  )

(defun +jg-tag-occurrences ()
  " Create a Bar Chart of Tags in the current buffer "
  (interactive)
  (let* ((depth-arg evil-ex-argument)
         (depth (if depth-arg (string-to-number depth-arg) nil))
         (alltags (make-hash-table :test 'equal))
         )
    (if (eq 'org-mode major-mode)
        (progn
          ;; (message "Getting Tags for all buffers to depth: %s" depth)
          (maphash (lambda (k v) (incf (gethash k alltags 0) v)) (+jg-tag-get-buffer-tags nil depth))
          (if (not (hash-table-empty-p alltags))
              (+jg-tag-chart-tag-counts alltags (buffer-name))
            (message "No Tags in buffer")))
      (message "Not in an org buffer")
      )
    )
  )
(defun +jg-tag-occurrences-in-open-buffers()
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
                         (cl-incf (gethash k alltags) v)) (+jg-tag-get-buffer-tags x depth))
            )
          )
    (if (not (hash-table-empty-p alltags))
        (+jg-tag-chart-tag-counts alltags "Active Files")
      (message "No Tags in buffers"))
    )
  )

(defun +jg-tag-org-tagged-p  (filename)
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

;; TODO set-tags-and-repeat
(defun +jg-tag-set-tags (x)
  "Utility action to set tags. Works in org *and* bibtex files"
  (if (gethash major-mode jg-tag-alt-mapping)
      (funcall (car (gethash major-mode jg-tag-alt-mapping)) x)
    (message "No Tag Handler found for Mode: %s" major-mode)
  )
(defun +jg-tag-set-new-tag (x)
  "Utility action to add a new tag. Works for org *and* bibtex"
  (if (gethash major-mode jg-tag-alt-mapping)
      (funcall (cdr (gethash major-mode jg-tag-alt-mapping)) x)
    (message "No Tag Handler found for Mode: %s" major-mode)
  )


(defun +jg-tag-select-random-tags (n)
  (interactive "nHow many tags? ")
  (let* ((tags (hash-table-keys jg-tag-global-tags))
         (selection (mapcar (lambda (x) (seq-random-elt tags)) (make-list n ?a)))
         )
    (with-temp-buffer-window "*Rand Tags*"
                             'display-buffer-pop-up-frame
                             nil
                             (mapc (lambda (x) (princ x ) (princ "\n")) selection)
                             )
    )
  )
