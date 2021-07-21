;;; util/+jg-bibtex/+hooks.el -*- lexical-binding: t; -*-

;; Clean bibtex hooks:
;; adapted from org-ref/org-ref-core.el: orcb-key-comma
;; For org-ref-clean-bibtex-entry-hook
(defun +jg-bibtex-dont-break-lines-hook()
  "Fix File paths and URLs to not have linebreaks"
  (bibtex-beginning-of-entry)
  (beginning-of-line)
  (let* ((keys (mapcar #'car (bibtex-parse-entry)))
         (paths (-filter #'(lambda (x) (string-match jg-bibtex-remove-field-newlines-regexp x)) keys))
         (path-texts (mapcar #'bibtex-text-in-field paths))
         (path-cleaned (mapcar #'(lambda (x) (replace-regexp-in-string "\n +" " " x)) path-texts))
         )
    ;; Then update:
    (mapc #'(lambda (x) (bibtex-set-field (car x) (cdr x))) (-zip paths path-cleaned))
    )
  )
(defun +jg-bibtex-clean-doi-hook ()
  "Remove http://dx.doi.org/ in the doi field."
  (let ((doi (bibtex-autokey-get-field "doi")))
    (when (ffap-url-p  doi)
      (bibtex-beginning-of-entry)
      (goto-char (car (cdr (bibtex-search-forward-field "doi" t))))
      (bibtex-kill-field)
      (bibtex-make-field "doi")
      (backward-char)
      (insert (replace-regexp-in-string "^http.*?\.org/" "" doi)))))
(defun +jg-bibtex-align-hook ()
  (let (start end)
    (bibtex-beginning-of-entry)
    (setq start (line-beginning-position 2))
    (bibtex-end-of-entry)
    (setq end (line-end-position 0))
    (align-regexp start end "\\(\s+?\\)=" 1 1 nil)
    (bibtex-end-of-entry)
    (setq end (line-end-position 0))
    (align-regexp start end "\\(.+?=\\)\\(\s+?\\)[{0-9\"]" 2 1 nil)
    )
)

(defun +jg-bibtex--get-file-entries (pair)
  (if (string-match "file" (car pair))
      pair
    nil)
  )
(defun +jg-bibtex--check-file-exists (pair)
  (let ((orig (cdr pair))
        (sub (substring (cdr pair) 1 -1)))
    (assert (eq (string-to-char orig) ?{))
    (if (not (f-exists? sub))
        (signal 'error `("File Not Found: " ,sub))
      )
    )
  )
(defun +jg-bibtex-check-file-hook ()
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
        (file-likes (-filter 'identity (mapcar #'+jg-bibtex--get-file-entries entry)))
        )
    (mapc #'+jg-bibtex--check-file-exists file-likes)
    )
  )

(defun +jg-bibtex-fix-paths (pair)
  `(,(car pair)
    .
    ,(+jg-bibtex--recode-non-ascii (substring (cdr pair) 1 -1))))
(defun +jg-bibtex-set-pair (pair)
  (bibtex-set-field (car pair) (cdr pair))
  )
(defun +jg-bibtex--recode-non-ascii (x)
  ;; jg-bibtex-non-ascii-inverted
  ;; BUG can't handle {\\c{s}}
  (with-temp-buffer
    (insert x)
    (goto-char (point-min))
    (let (last-match replace-data)
      (while (re-search-forward "{.+?}" nil t)
        (setq replace-data (assoc (match-string 0) jg-bibtex-non-ascii-inverted))
        (if replace-data
            (replace-match (cdr replace-data) nil nil nil 0)
          )
        )
      (buffer-string)
      )
    )
  )


(defun +jg-bibtex-recode-files-hook ()
  (bibtex-beginning-of-entry)
  (let* ((entry (bibtex-parse-entry))
         (file-likes (-filter 'identity (mapcar #'+jg-bibtex--get-file-entries entry)))
         (fixed (mapcar #'+jg-bibtex-fix-paths file-likes))
         )
    (mapc #'+jg-bibtex-set-pair fixed)
    )
  )

(defun +jg-bibtex-smart-replace-nonascii-hook ()
  "Hook function to replace non-ascii characters in a bibtex entry."
  (interactive)
  (save-restriction
    (bibtex-narrow-to-entry)
    (goto-char (point-min))
    (dolist (char (mapcar (lambda (x)
			    (car x))
			  org-ref-nonascii-latex-replacements))
      (while (re-search-forward char nil t)
        (if (not (string-match "file.*?=" (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (replace-match (cdr (assoc char org-ref-nonascii-latex-replacements)))
          (goto-char (line-end-position))
          )
        )
      (goto-char (point-min)))))

(defun +jg-bibtex-orcb-key-hook (&optional allow-duplicate-keys)
  "Replace the key in the entry.
Prompts for replacement if the new key duplicates one already in
the file, unless ALLOW-DUPLICATE-KEYS is non-nil."
  (let ((key (funcall org-ref-clean-bibtex-key-function
		      (bibtex-generate-autokey))))
    ;; remove any \\ in the key
    (setq key (replace-regexp-in-string "\\\\" "" key))
    ;; first we delete the existing key
    (bibtex-beginning-of-entry)
    (re-search-forward bibtex-entry-maybe-empty-head)
    (if (match-beginning bibtex-key-in-head)
	(delete-region (match-beginning bibtex-key-in-head)
		       (match-end bibtex-key-in-head)))
    ;; check if the key is in the buffer
    (when (and (not allow-duplicate-keys)
               (save-excursion
                 (bibtex-search-entry key)))
      (save-excursion
	(bibtex-search-entry key)
	(bibtex-copy-entry-as-kill)
	(switch-to-buffer-other-window "*duplicate entry*")
	(bibtex-yank))
      (setq key (bibtex-read-key "Duplicate Key found, edit: " key)))

    (insert key)
    (kill-new key)))


(defun +jg-bibtex-insert-stub-key ()
  (bibtex-beginning-of-entry)
  (search-forward "{" (line-end-position) t)
  (if (looking-at ",")
      (insert (format "stub_key_%s" (random 5000)))
    )
  )

(defun +jg-bibtex-insert-volume-to-key ()
  (bibtex-beginning-of-entry)
  (let ((vol (bibtex-autokey-get-field "volume")))
    (if vol
        (progn
          (goto-char (- (line-end-position) 1))
          (insert (format "_%s" vol))
        )
      )
    )
  )
