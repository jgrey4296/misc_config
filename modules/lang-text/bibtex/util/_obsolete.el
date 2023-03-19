;;; +stubbing.el -*- lexical-binding: t; -*-


(defun +jg-bibtex-dired-stub-entries ()
  " Discover all pdfs in a directory, create stubs for them "
  (interactive)
  (let* ((curr-dir (dired-current-directory))
         (files    (f-files curr-dir  (lambda (x) (or (f-ext? x "pdf") (f-ext? x "epub")))))
         (target-bib (read-file-name "Todo-bib: " jg-bibtex-loc-bibtex))
         mentioned
         )
    ;; Get mentioned
    (unless (f-exists? target-bib)
      (error "Target Stub File doesn't exist: %s" target-bib))
    (with-temp-buffer
      (insert-file-contents target-bib))
    (goto-char (point-min))
    (while (re-search-forward "^\s*file[0-9]*\s*=\s*{\\(.+?\\)}" nil t)
      (cl-pushnew (match-string 1) mentioned :test 'equal)
      )
    (goto-char (point-max))
    (insert "\n")
    (message "Found: %s\n Mentioned: %s\n Remaining: %s"
             (length files) (length mentioned) (length (-difference files
                                                                    mentioned)))
    (cl-loop with count = 0
          for file in (-difference files mentioned)
          do
          (insert (format "@Misc{stub_%s,\n" (int-to-string count))
                  (format "  year = {%s},\n" (nth 2 (calendar-current-date)))
                  (format "  title = {%s},\n" (f-no-ext (f-filename file)))
                  (format "  file = {%s},\n"  file)
                  "}\n")
          (cl-incf count)
          )
    (write-file target-bib)
    )
  )
