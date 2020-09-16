(defun tag-clean/strip_spaces (str)
  "Replace spaces with underscores"
  (s-replace " " "_" (string-trim str))
  )


(defun tag-clean/mark-to-filter ()
  "Utility to quickly add tag"
  (interactive)
  (search-forward ":")
  (evil-delete-line (point) (line-end-position))
  (insert " __filter__")
  (next-line)
  (goto-char (line-beginning-position))
  )

(defun tag-clean/mark-to-sub ()
  "Utility to quckly substitute a tag"
  (interactive)
  (search-forward ":")
  (evil-delete-line (point) (line-end-position))
  (insert " ")
  (insert (tag-clean/strip_spaces (read-string "Sub with: ")))
  (next-line)
  (goto-char (line-beginning-position))
  )

(defun tag-clean/leave ()
  "Utility to quickly add a leave annotation"
  (interactive)
  (search-forward ":")
  (evil-delete-line (point) (line-end-position))
  (insert " __leave__")
  (next-line)
  (goto-char (line-beginning-position))
)

(defun tag-clean/previous ()
  (interactive)
  (next-line -1)
  (goto-char (line-beginning-position))
  )

(define-minor-mode tag-clean-minor-mode
  "A minor mode to simplify choosing to filter or sub tags"
  :lighter "Tag-Clean"
  )

(provide tag-clean-minor-mode)
