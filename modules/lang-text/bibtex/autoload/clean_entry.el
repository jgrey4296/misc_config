;;; +entry.el -*- lexical-binding: t; -*-
(require 'bibtex)

;;;###autoload
(defun +jg-bibtex-smart-replace-nonascii-hook ()
  "Replace non-ascii characters in a bibtex entry.
but not in file or url entries
"
  (interactive)
    (goto-char (point-min))
    (dolist (char (mapcar (lambda (x)
			    (car x))
			  org-ref-nonascii-latex-replacements))
      (while (re-search-forward char nil t)
        (if (not (string-match "\\(file\\|url\\|doi\\).*?=" (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (replace-match (cdr (assoc char org-ref-nonascii-latex-replacements)))
          (goto-char (line-end-position))
          )
        )
      (goto-char (point-min))))

;;;###autoload
(defun +jg-bibtex-align-hook ()
  " Aligns a bibtex entry's '{field} =' "
  (let (start end)
    (bibtex-beginning-of-entry)
    (setq start (line-beginning-position 2))
    (bibtex-end-of-entry)
    (setq end (line-end-position 0))
    (align-regexp start end "\\(\s+?\\)[a-z]" 1 1 nil)
    (align-regexp start end "\\(\s+?\\)=" 1 1 nil)
    (bibtex-end-of-entry)
    (setq end (line-end-position 0))
    (align-regexp start end "\\(.+?=\\)\\(\s+?\\)[{0-9\"]" 2 1 nil)
    )
)

;;;###autoload
(defun +jg-bibtex-indent-hook ()
  " Indent all fields to jg-bibtex-indent-equals-column "
  (bibtex-beginning-of-entry)
  (while (re-search-forward "^.+?= {" nil t)
    (backward-char 3)
    (indent-to-column jg-bibtex-indent-equals-column)

    )
  )

;;;###autoload
(defun +jg-bibtex-bibtex-entry-commas ()
  " Ensure all fields have a comma at the end of the line"
  (bibtex-beginning-of-entry)
  (end-of-line)
  (if (not (looking-at-p ","))
      (insert ","))
  (while (re-search-forward "= {.+}$" nil t)
    (insert ",")
    )
  )

;;;###autoload
(defun +jg-bibtex-orcb-& ()
  "Replace naked & with \& in a bibtex entry.
Replace &amp; as well
But not in urls
"
  (bibtex-beginning-of-entry)
  (let* ((keys (mapcar #'car (bibtex-parse-entry)))
         (focus (-filter #'(lambda (x) (not (string-match "=\\|url\\|doi\\|file" x))) keys))
         (texts (mapcar #'bibtex-autokey-get-field focus))
         (cleaned (mapcar #'(lambda (x) (replace-regexp-in-string " & " " \\\\& " x)) texts))
         )
    ;; Then update:
    (mapc #'(lambda (x) (bibtex-set-field (car x) (cdr x))) (-zip-pair focus cleaned))
    )
  )

;;;###autoload
(defun +jg-bibtex-dont-break-lines-hook()
  " Remove newlines from entries "
  (bibtex-beginning-of-entry)
  (beginning-of-line)
  (let* ((entry (bibtex-parse-entry))
         (keys (mapcar #'car entry))
         (paths (-reject #'(lambda (x) (string-match jg-bibtex-remove-field-newlines-regexp x)) keys))
         (path-texts (mapcar #'bibtex-autokey-get-field paths))
         (path-cleaned (mapcar #'(lambda (x) (replace-regexp-in-string "\n+ *" " " x)) path-texts))
         )
    ;; Then update:
    (mapc #'(lambda (x) (bibtex-set-field (car x) (cdr x))) (-zip-pair paths path-cleaned))
    )
  )

;;;###autoload
(defun +jg-bibtex-clean-whitespace-hook()
  " Remove newlines from entries "
  (bibtex-beginning-of-entry)
  (beginning-of-line)
  (let* ((entry (bibtex-parse-entry))
         (main (-reject #'(lambda (x) (s-contains? "=" (car x))) entry))
         (cleaned (mapcar #'(lambda (x) (cons (car x) (s-trim (substring (cdr x) 1 -1)))) main))
         )
    ;; Then update:
    (mapc #'(lambda (x) (bibtex-set-field (car x) (cdr x))) cleaned)
    )
  )

;;;###autoload
(defun +jg-bibtex-remove-empty-fields ()
  (bibtex-beginning-of-entry)
  (while (re-search-forward "\\(ALT\\|OPT\\).+= {},?$" nil t)
    (kill-region (line-beginning-position) (line-end-position))
    (join-line)
    )
  )

;;;###autoload
(defun +jg-bibtex-reformat-buffer (&optional read-options)
  "Reformat all BibTeX entries in buffer or region.
Without prefix argument, reformatting is based on `bibtex-entry-format'.
With prefix argument, read options for reformatting from minibuffer.
With \\[universal-argument] \\[universal-argument] prefix argument, reuse previous answers (if any) again.
If mark is active reformat entries in region, if not in whole buffer."
  (interactive "*P")
  (let* ((pnt (point))
         )
    (save-restriction
      (if mark-active (narrow-to-region (region-beginning) (region-end)))
      (bibtex-progress-message "Formatting" 1)
      (bibtex-map-entries (lambda (_key _beg _end)
                            (bibtex-progress-message)
                            (org-ref-clean-bibtex-entry)))
      (bibtex-progress-message 'done))
    (goto-char pnt)))
