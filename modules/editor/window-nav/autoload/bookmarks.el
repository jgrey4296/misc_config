;;; bookmarks.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-nav-bookmarks-load()
  (interactive)
  (bookmark-load (read-file-name "Load bookmarks file: " (expand-file-name "templates/bookmarks/" doom-user-dir)
                                 (pcase system-type
                                   ('darwin "bookmarks.mac")
                                   ('gnu/linux "bookmarks.linux")
                                   )
                                 t "bookmarks") t)
  )

;;;###autoload
(defun +jg-nav-open-link ()
  (interactive)
  (cond ((eq evil-state 'visual)
         (let ((str (buffer-substring-no-properties evil-visual-beginning evil-visual-end)))
           (org-open-link-from-string (format "[[%s]]" (string-trim str)))
           ))
        (t (org-open-at-point 'in-emacs))
        )
  )

;;;###autoload
(defun +jg-nav-open-link-externally ()
  (interactive)
  (let ((current-prefix-arg '(16))
        (str (if (eq evil-state 'visual) (buffer-substring-no-properties evil-visual-beginning evil-visual-end) nil))
        )
    (cond ((eq evil-state 'visual)
           (funcall-interactively 'org-open-link-from-string (format "[[%s]]" (string-trim str))))
          ((eq 'link (org-element-type (org-element-context)))
           (call-interactively 'org-open-at-point))
          (t
           (funcall-interactively 'org-open-link-from-string
                                  (format "[[%s]]" (string-trim (buffer-substring-no-properties (line-beginning-position)
                                                                                                (line-end-position))))))
          )
    )
  )
