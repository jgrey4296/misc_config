;;; +windows.el -*- lexical-binding: t; -*-
(require 'bookmark)
(require 'bibtex)

;;;###autoload
(defun +jg-bibtex-window-set-downloads ()
  (interactive)
  (let* ((top-wind (split-window-right))
         (bot-wind (with-selected-window top-wind
                     (split-window-below)))
         )
    (with-selected-window top-wind
      (bookmark-jump "downloads")
      )
    (with-selected-window bot-wind
      (bookmark-jump "todo_pdfs")
      )
    )
  )

;;;###autoload
(defun +jg-bibtex-window-set-dropbox()
  (interactive)
  (let* ((top-wind (split-window-right))
         (bot-wind (with-selected-window top-wind
                     (split-window-below)))
         )
    (with-selected-window top-wind
      (bookmark-jump "dropbox")
      )
    (with-selected-window bot-wind
      (bookmark-jump "todo_pdfs")
      )
    )
  )

;;;###autoload
(defun +jg-bibtex-window-file-folder ()
  " Find the folder in which the entry's associated file exists "
  (interactive)
  (let* ((target (bibtex-autokey-get-field '("file" "OPTfile"))))
    (when (and (not (string-empty-p target)) (f-exists? (f-parent target)))
      (message "Opening %s" (f-parent target))
      (find-file-other-window (f-parent target))
      (goto-char (point-min))
      (search-forward (f-filename target))
      t
      )
    )
  )

;;;###autoload
(defun +jg-bibtex-window-dwim ()
  (interactive)
  (let ((entry-start (save-excursion (bibtex-beginning-of-entry) (point)))
        (entry-end (save-excursion (bibtex-end-of-entry) (point)))
        )
    (if (and (<= entry-start (point))
             (<= (point) entry-end))
        (unless (+jg-bibtex-window-file-folder)
          (+jg-bibtex-window-set-downloads))
      (+jg-bibtex-window-set-downloads))
    )
  )
