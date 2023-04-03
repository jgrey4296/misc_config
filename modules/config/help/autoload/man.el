;;; man.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-help-man ()
  (interactive)
  (with-file-contents! (f-join doom-local-dir "man-completions")
    (let ((result (ivy-read "Man Page: " (s-split "\n" (buffer-string) t))))
      (man (car (s-split "\\( \\|,\\)" result)))
      )
    )
  )

;;;###autoload
(defun +jg-help-man-completion-build ()
  (call-process "man" nil `(:file ,(f-join doom-local-dir "man-completions")) nil "-k" ".")
  )
