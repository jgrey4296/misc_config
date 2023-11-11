;; font-examples.el -*- mode: emacs-lisp; lexical-binding: t; -*-

(defvar jg-latex-font-example-template (expand-file-name  "templates/tex-config/tex/font_template.tex" doom-user-dir))
(defvar jg-latex-font-pattern "!!!FONT!!!")

;;;###autoload
(defun +jg-latex-dired-build-font-examples ()
  " For marked font files, build an example tex file to compile "
  (interactive)
  (let ((fonts (dired-get-marked-files 'local))
        (loaded-template (with-temp-buffer
                           (insert-file-contents-literally jg-latex-font-example-template)
                           (buffer-string)
                           ))
        )
    (dolist (font fonts)
      (with-temp-buffer
        (insert loaded-template)
        (goto-char (point-min))
        (while (re-search-forward jg-latex-font-pattern nil t)
          (replace-match font)
          )
        (write-file (expand-file-name (format "%s.tex" font) default-directory))
        )
      )
    (dired-unmark-all-marks)
    (dired-mark-files-regexp "\.tex")
    )
  )
