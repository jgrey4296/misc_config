;;; lang/jg-org/+tags.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-org-set-tags (x)
  " Improved action to add and remove tags Toggle Selected Tags
Can operate on regions of headings "
  (let* ((actual-candidates (mapcar 'car (helm-marked-candidates)))
         (prior-point 1)
         (end-pos librarian-tagging-mode-marker)
         (heading-re (if (org-property-values "TWITTER-BUFFER")
                         "^\*\* Thread"
                       "^\*"
                       )))

    (cond ((eq evil-state 'normal)
           (save-excursion
             (if (or (looking-at heading-re) (re-search-backward heading-re nil t))
                 (+jg-org-integrate-tags actual-candidates))
             ))
          ((eq evil-state 'visual)
           (save-excursion
             (setq prior-point (- (point) 1))
             (re-search-forward heading-re end-pos t)
             (while (and (/= prior-point (point)) (< (point) end-pos))
               (progn (setq prior-point (point))
                      (+jg-org-integrate-tags actual-candidates)
                      (org-forward-heading-same-level 1)))))
          (t (message "Unknown Tagging State")))))

;;;###autoload
(defun +jg-org-set-new-tag (x)
  "Utility to set a new tag for an org heading"
  (let ((prior-point (- (point) 1))
        (end-pos librarian-tagging-mode-marker)
        (stripped-tag (+jg-text-strip-spaces x))
        (heading-re (if (org-property-values "TWITTER-BUFFER")
                        "^\*\* Thread"
                      "^\*"
                      )))

      (cond ((eq evil-state 'normal)
             (save-excursion
               (if (or (looking-at heading-re) (re-search-backward heading-re nil t))
                   (+jg-org-integrate-tags (list stripped-tag)))
               ))
            ((eq evil-state 'visual)
             (save-excursion
               (goto-char prior-point)
               (re-search-forward heading-re nil t)
               (while (and (/= prior-point (point)) (< (point) end-pos))
                 (setq prior-point (point))
                 (+jg-org-integrate-tags (list stripped-tag))
                 (org-forward-heading-same-level 1))))
            (t (message "Unknown Tagging State")))))

(defun +jg-org-integrate-tags (xs)
  (let ((current-tags (org-get-tags nil t)))
    (cl-loop for tag in xs do
          (if (not (-contains? current-tags tag))
              (push tag current-tags)
            (setq current-tags (remove tag current-tags))
            )
          )
    (org-set-tags nil)
    (org-set-tags (sort current-tags #'string-lessp))
    )
)

;;;###autoload
(defun +jg-org-get-buffer-tags (&optional name depth)
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

(defun +jg-org-get-file-tags (filename &optional depth)
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

(defun +jg-org-tagged-p  (filename)
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

(defun +jg-org-format-temp-buffer (bufname name)


  )
