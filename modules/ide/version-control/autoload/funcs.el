;;; +funcs.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-vcs-insert-tag (&optional arg)
  (interactive "P")
  (let* ((alltags (with-temp-buffer (insert-file jg-vcs-tag-file) (sort (s-split "\n" (buffer-string) t) #'string-lessp)))
         (tag (ivy-read "VCS Tag: " alltags))
         )
    (unless (-contains? alltags tag)
      (append-to-file (format "\n%s" tag) nil jg-vcs-tag-file)
      )

    (goto-char (point-min))
    (insert "[" tag "]")
    (insert (if arg "!: " ": "))
    )
  )
