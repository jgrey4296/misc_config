;;; util/jg-tag/+hooks.el -*- lexical-binding: t; -*-

;; Clean bibtex hooks:
;; adapted from org-ref/org-ref-core.el: orcb-key-comma
;; For org-ref-clean-bibtex-entry-hook
(defun jg-tag-dont-break-lines-hook()
  "Fix File paths and URLs to not have linebreaks"
  (bibtex-beginning-of-entry)
  (beginning-of-line)
  (let* ((keys (mapcar #'car (bibtex-parse-entry)))
         (paths (-filter #'(lambda (x) (string-match jg-tag-remove-field-newlines-regexp x)) keys))
         (path-texts (mapcar #'bibtex-text-in-field paths))
         (path-cleaned (mapcar #'(lambda (x) (replace-regexp-in-string "\n +" " " x)) path-texts))
         )
    ;; Then update:
    (mapc #'(lambda (x) (bibtex-set-field (car x) (cdr x))) (-zip paths path-cleaned))
    )
  )
(defun jg-orcb-clean-doi ()
  "Remove http://dx.doi.org/ in the doi field."
  (let ((doi (bibtex-autokey-get-field "doi")))
    (when (ffap-url-p  doi)
      (bibtex-beginning-of-entry)
      (goto-char (car (cdr (bibtex-search-forward-field "doi" t))))
      (bibtex-kill-field)
      (bibtex-make-field "doi")
      (backward-char)
      (insert (replace-regexp-in-string "^http.*?\.org/" "" doi)))))
(defun jg-bibtex-align ()
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
