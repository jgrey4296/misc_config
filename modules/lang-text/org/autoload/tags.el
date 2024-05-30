;;; lang/jg-org/+tags.el -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'librarian-tagging)

;;;###autoload
(cl-defmethod librarian-set-tags ((mode (eql 'org-mode)) add sub keep)
  (let ((joined(append add keep))
         )
    (org-set-tags joined)
    )
  )

;;;###autoload
(cl-defmethod librarian-set-new-tags ((mode (eql 'org-mode)) new)
  "Utility to set a new tag for an org heading"
  (org-set-tags new)
  )

;;;###autoload
(cl-defmethod librarian-get-tags ((mode (eql 'org-mode)))
  (org-get-tags nil t)
  )

;;;###autoload
(cl-defmethod librarian-get-buffer-tags ((mode (eql 'org-mode)))
  (org-get-buffer-tags)
  )

;;;###autoload
(cl-defmethod librarian-backward-entry ((mode (eql 'org-mode)))
  (evil-backward-section-begin)
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
  (user-error "TODO")
  )
