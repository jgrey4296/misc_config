;;; +transformers.el -*- lexical-binding: t; -*-


(defun +jg-bibtex-helm-candidates-formatter (candidates _)
  "Format CANDIDATES for display in helm."
  (cl-loop
   with width = (with-helm-window (helm-bibtex-window-width))
   for entry in candidates
   for entry = (cdr entry)
   for entry-key = (bibtex-completion-get-value "=key=" entry)
   collect (cons (+jg-bibtex-completion-format-entry entry width) entry-key))
)

(defun +jg-bibtex-completion-format-entry (entry width)
  "Formats a BibTeX ENTRY for display in results list.
WIDTH is the width of the results list.  The display format is
governed by the variable `bibtex-completion-display-formats'."
  (let* ((entry-type (bibtex-completion-get-value "=type=" entry))
         (entry-format (cdr (or (assoc-string entry-type jg-bibtex-completion-display-formats 'case-fold)
                                (assoc t jg-bibtex-completion-display-formats))))
         )
    (s-format
     (car entry-format)
     (lambda (field)
       (let* ((field (split-string field ":"))
              (field-name (car field))
              (field-width (cond ((and (cadr field) (string= (cadr field) "*"))
                                  0)
                                 ((cadr field)
                                  (string-to-number (cadr field)))
                                 (t 0)
                                 ))
              (field-value (bibtex-completion-clean-string (or (cond ((string= field-name "author")
                                                                      (bibtex-completion-shorten-authors (or (bibtex-completion-get-value field-name entry)
                                                                                                             (bibtex-completion-get-value "editor" entry))))
                                                                     ((string= field-name "year")
                                                                      (or (bibtex-completion-get-value field-name entry)
                                                                          (car (split-string (bibtex-completion-get-value "date" entry "") "-"))))
                                                                     (t (bibtex-completion-get-value field-name entry)))
                                                               "")))
              )
         (when (and field-width (<= field-width 0))
           (setq field-width (- width (cadr entry-format))))
         (when field-width
           (setq field-value (truncate-string-to-width field-value field-width 0 ?\s t)))

         field-value
         )))))
(defun +jg-bibtex-process-candidates (x)
  "Utility to tidy bibtex-completion-candidates for helm-bibtex"
  (cons (s-replace-regexp ",? +" " " (car x))
        (cdr x))
  )

(defun +jg-bibtex-sort-by-year (c1 c2)
  (let* ((c1year (alist-get "year" c1 nil nil 'string-equal))
         (c2year (alist-get "year" c2 nil nil 'string-equal)))
    (if (not c1year)
        (progn (message "MISSING YEAR: %s" c1)
               (setq c1year "0")))
    (if (not c2year)
        (progn (message "MISSING YEAR: %s" c2)
               (setq c2year "0")))
    (> (string-to-number c1year) (string-to-number c2year))
    )
  )
(defun +jg-bibtex-year-sort-transformer (candidates source)
  (-sort #'+jg-bibtex-sort-by-year candidates)
  )
(defun +jg-bibtex-edit-finish (cand)
  (let ((marked-cands (helm-marked-candidates)))
    (if marked-cands
        marked-cands
      (list cand)
      )))
