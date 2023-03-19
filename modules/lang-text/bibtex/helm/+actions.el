;;; +actions.el -*- lexical-binding: t; -*-


;;-- actions
(defun +jg-bibtex-show-entry (keys)
  "Show the first entry in KEYS in the relevant BibTeX file.
modified from the original bibtex-completion-show-entry
"
  (let* ((bib-loc jg-bibtex-loc-bibtex)
         (entry (bibtex-completion-get-entry keys))
         (year (bibtex-completion-get-value "year" entry))
         (year-file (f-join bib-loc (format "%s.bib" year)))
         (todo-file jg-bibtex-todo-loc)
         )
    (catch 'break
      (dolist (bib-file `(,year-file ,todo-file))
        (find-file bib-file)
        (if (buffer-narrowed-p)
            (widen))
        (goto-char (point-min))
        ;; find the key
        (bibtex-find-entry keys)
        (let ((bounds (+evil:defun-txtobj)))
          (narrow-to-region (car bounds) (cadr bounds))
          (throw 'break t)
          )))))
(defun +jg-bibtex-edit-notes (keys)
  "Show the first entry in KEYS in the relevant BibTeX file.
modified from the original bibtex-completion-show-entry
"
  (let* ((bib-loc jg-bibtex-loc-bibtex)
         (entry (bibtex-completion-get-entry keys))
         (year (bibtex-completion-get-value "year" entry))
         (year-file (f-join bib-loc (format "%s.bib" year)))
         (todo-file jg-bibtex-todo-loc)
         )
    (catch 'break
      (dolist (bib-file `(,year-file ,todo-file))
        (find-file bib-file)
        (if (buffer-narrowed-p)
            (widen))
        ;; find the key
        (bibtex-find-entry keys)
        (let ((bounds (bibtex-search-forward-field "notes" t)))
          ;; create notes if not existing
          (if (not bounds)
              (progn
                (bibtex-end-of-entry)
                (evil-open-above 0)
                (insert " notes = {")
                (setq bounds `(nil ,(point) ,(+ (point) 2) ,(+ (point) 2)))
                (insert " }")
                ))
          (narrow-to-region (nth 1 bounds) (nth 2 bounds)))
        (org-mode)
        (throw 'break t)
        ))))
(defun +jg-bibtex-insert-simple(x)
  (let* ((entry (bibtex-completion-get-entry x))
         (name (cdr (assoc "title" entry 's-equals?)))
         )
    (insert name)
    )
  )
(defun +jg-bibtex-insert-wrapped ()
  (interactive)
  (save-excursion
    (let ((curr-word (current-word)))
      (evil-end-of-line)
      (insert " ")
      (+jg-bibtex-insert-simple curr-word)
      )
    )
  )
(defun +jg-bibtex-helm-tweet-action (x)
  (let* ((entry (bibtex-completion-get-entry x)))

    (+jg-twitter-tweet-with-input (format jg-bibtex-tweet-pattern
                                          (alist-get "year" entry nil nil #'equal)
                                          (alist-get "title" entry nil nil #'equal)
                                          (alist-get "author" entry nil nil #'equal)
                                          (alist-get "tags" entry nil nil #'equal)
                                          (or (alist-get "doi" entry nil nil #'equal)
                                              (alist-get "url" entry nil nil #'equal)
                                              (alist-get "isbn" entry nil nil #'equal))
                                          )))
  )
(defun +jg-bibtex-helm-open-files(key)
  (cl-loop for cand in (helm-marked-candidates)
        do
        (let* ((entry (bibtex-completion-get-entry cand))
               (target (+jg-bibtex-file-expand (bibtex-completion-get-value "file" entry)))
               )
          (message "Opening %s" target)
          (+jg-bibtex-open-pdf target)
          )
        )
  )
;;-- end actions
