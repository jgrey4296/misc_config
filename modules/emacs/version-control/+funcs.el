;;; +funcs.el -*- lexical-binding: t; -*-

(defun +jg-vcs-insert-tag ()
  (interactive)
  (let* ((alltags (with-temp-buffer (insert-file jg-vcs-tag-file) (sort (s-split "\n" (buffer-string) t) #'string-lessp)))
         (tag (ivy-read "VCS Tag: " alltags))
         )
    (unless (-contains? alltags tag)
      (append-to-file (format "\n%s" tag) nil jg-vcs-tag-file)
      )

    (goto-char (point-min))
    (insert "[" tag "]" )
    )

  )
