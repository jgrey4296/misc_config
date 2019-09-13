(defun tag-clean/strip_spaces (str)
  (s-replace " " "_" (string-trim str))
  )


(defun tag-clean/mark-to-filter ()
  (interactive)
  (search-forward ":")
  (evil-delete-line (point) (line-end-position))
  (insert " __filter__")
  (next-line)
  (goto-char (line-beginning-position))
  )

(defun tag-clean/mark-to-sub ()
  (interactive)
  (search-forward ":")
  (evil-delete-line (point) (line-end-position))
  (insert " ")
  (insert (tag-clean/strip_spaces (read-string "Sub with: ")))
  (next-line)
  (goto-char (line-beginning-position))
  )

(defun tag-clean/leave ()
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
