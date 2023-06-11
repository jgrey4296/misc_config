;;; ivy.el -*- lexical-binding: t; -*-

(defvar jg-markdown-post-loc "/Volumes/documents/github/jgrey4296.github.io/posts")

(defvar jg-markdown-post--cache nil)

;;;###autoload
(defun +jg-markdown-post-ivy ()
  (interactive)
  (unless (and jg-markdown-post--cache (hash-table-count jg-markdown-post--cache))
    (setq jg-markdown-post--cache (make-hash-table :test 'equal))
    (cl-loop for file in (f-entries jg-markdown-post-loc #'(lambda (x) (f-ext? x "md")) t)
             do
             (let ((dir (f-base (f-parent file))))
               (puthash (f-join dir (f-base file)) file jg-markdown-post--cache)
               ))
    )

  (let* ((chosen (ivy-read "Select Module Bindings: " jg-markdown-post--cache :require-match t))
         (post (gethash chosen jg-markdown-post--cache))
         )
    (if (f-exists? post)
        (find-file post)
      (message "Doesnt Exist: %s" post)
      )
    )
)
