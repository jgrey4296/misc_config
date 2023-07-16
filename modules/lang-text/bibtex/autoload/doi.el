;;; doi.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-bibtex-insert-entry-from-doit ()
  (interactive)
  (let* ((doi (read-string "Doi: "))
         (results (funcall doi-utils-metadata-function doi))
         (type (plist-get results :type))
         (bibtex (-some (lambda (g) (funcall g type results)) doi-utils-bibtex-type-generators))
         )
    (cond ((not bibtex)
           (insert "@misc{,\n")
           (while results
             (let ((key (string-replace ":" "" (symbol-name (pop results))))
                   (value (pop results)))
               (if (not (vectorp value))
                   (insert (format "%s = {%s},\n" key value))
                 (dolist (x (append value nil))
                   (insert (format "%s = {%s},\n" key (-remove #'keywordp x)))
                   )
                 )
               )
             )
           (insert "}")
           )
          (t
           (insert bibtex)
           (backward-char)
           ;; set date added for the record
           (let ((ts (funcall doi-utils-timestamp-format-function)))
             (when ts
               (bibtex-set-field doi-utils-timestamp-field
			         ts)))
           (org-ref-clean-bibtex-entry)
           (save-buffer))
          )
    )
  )
