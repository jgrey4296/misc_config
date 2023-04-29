;;; cleaning.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-text-strip-spaces (str)
  "Utility to replace spaces with underscores in a string.
Used to guard inputs in tag strings"
  (s-replace " " "_" (string-trim str))
  )

;;;###autoload
(defun +jg-text-remove-leading-whitespace ()
  (interactive)
  (let ((start (if (evil-visual-state-p) evil-visual-beginning (line-beginning-position)))
        (end (if (evil-visual-state-p) evil-visual-end (line-end-position))))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (beginning-of-line-text)
        (kill-region (line-beginning-position) (point))
        (forward-line)
        )
      )
    )
  )

;;;###autoload
(defun +jg-text-cleanup-whitespace ()
  " compact multiple newlines into just one "
  (while (re-search-forward "\n\n\\(\n+\\)" nil t)
    (let ((matched (length (match-string 1))))
      (replace-match "" nil nil nil 1)
      )
    )
  )

;;;###autoload
(defun +jg-text-split-files-into-chapters ()
  " Split a group of marked files into separate chapter html files,
using a list of chapter titles"
  (interactive)
  (let* ((files (dired-get-marked-files))
         (title-file (read-file-name "Chapter File: " default-directory))
         (titles (with-file-contents! title-file
                   (split-string (buffer-string) "\n" t)))
         (current 1)
         start end substring
         )
    (message "Titles: %s" titles)
    (pop titles)
    (cl-loop for file in files
             do
             (with-file-contents! file
               (re-search-forward "<body class=\"calibre\">$" nil t)
               (forward-line) (beginning-of-line)
               (setq start (point))
               (while (and titles (search-forward (car titles) nil 1))
                 (message "Searched for: %s" (car titles))
                 (cond ((not start)
                        (message "Setting start")
                        (pop titles)
                        (cl-incf current)
                        (beginning-of-line)
                        (setq start (point))
                        )
                       (start
                        (beginning-of-line)
                        (setq end (point))
                        (message "Appending: %s" (s-trim (buffer-substring start end)))
                        (append-to-file start end (format "ch%s.html" current))
                        (append-to-file "\n" nil (format "ch%s.html" current))
                        (forward-line -1)
                        (setq start (if titles nil (point))
                              end nil)
                        ))
                 )
               (when start
                 (setq end (point-max))
                 (message "Final : %s" (s-trim (buffer-substring start end)))
                 (append-to-file start end (format "ch%s.html" current))
                 (append-to-file "\n" nil (format "ch%s.html" current))
                 (setq start nil
                       end nil)
                 )
               )
             )
    )
  )
