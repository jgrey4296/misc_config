;;; domain-specific/bibtex/+helm.el -*- lexical-binding: t; -*-

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

;;-- utils
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
;;-- end utils

;;-- helms
(defun +jg-bibtex-helm-bibtex (&optional arg local-bib)
  " Custom implementation of helm-bibtex"
  (interactive "P")
  (require 'helm-bibtex)
  (when arg
    (message "Clearing Bibtex File Cache")
    (+jg-bibtex-build-list)
    (bibtex-completion-clear-cache)
    (bibtex-completion-init)
    )
  (when (null jg-bibtex-helm-candidates)
    (message "Generating Candidates")
    (setq jg-bibtex-helm-candidates
          (mapcar '+jg-bibtex-process-candidates (bibtex-completion-candidates)))

    )
  (helm-set-local-variable 'helm-candidate-number-limit 5000)

  (let ((input (if (evil-visual-state-p) (buffer-substring-no-properties evil-visual-beginning evil-visual-end) ""))
        )

    (helm :sources `(,jg-bibtex-helm-source-bibtex)
          :full-frame t
          :buffer "*helm bibtex*"
          :input input
          :bibtex-local-bib local-bib
          :bibtex-candidates jg-bibtex-helm-candidates
          )))

(defun +jg-bibtex-edit-field ()
  " Edit a specified field in the current entry,
using org-bibtex-fields for completion options "
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((composition (-compose #'(lambda (x) (s-replace ":" "" x)) #'symbol-name #'car  ))
           (fields (sort (mapcar composition bibtex-field-alist) #'string-lessp))
           (chosen (completing-read "Field: " fields))
           (curr-value (bibtex-autokey-get-field chosen))
           (potential-completions (f-join jg-bibtex-loc-completions chosen))
           (source (if (f-exists? potential-completions)
                       (helm-build-in-file-source "Completion Helm"
                           potential-completions
                         :action (helm-make-actions "Accumulate" #'+jg-bibtex-edit-finish)
                         )))
           (dummy-action #'(lambda (x) (write-region (format "%s\n" (string-trim x)) nil potential-completions t) x))
           (dummy-source (helm-build-dummy-source "Completion Helm Dummy"
                           :action (helm-make-actions "Insert into file" dummy-action)))
           new-values
           )
      ;; TODO use author ivy
      ;; TODO repeat helm
      (setq new-values (if source
                          (helm :sources (list source dummy-source)
                                :buffer "*helm bibtex completions*"
                                :full-frame nil
                                :input curr-value
                                )
                          (read-string (format "(%s) New Value: " chosen))))
      (if new-values
          (bibtex-set-field chosen (if (listp new-values)
                                       (string-join (mapcar 'string-trim new-values) " and ")
                                     new-values) t)
        )
      )
    )
  )
;;-- end helms

;;-- sources
(after! helm
  ;; Define the bib helm

  (setq jg-bibtex-helm-source-bibtex
        (helm-build-sync-source "Bibtex Helm"
          :action (helm-make-actions  "Insert citation"      #'helm-bibtex-insert-citation
                                      "Open file"            #'+jg-bibtex-helm-open-files
                                      "Insert BibTeX key"    #'helm-bibtex-insert-key
                                      "Insert BibTeX entry"  #'helm-bibtex-insert-bibtex
                                      "Insert Bibtex simple" #'+jg-bibtex-insert-simple
                                      "Show entry"           #'+jg-bibtex-show-entry
                                      "Edit Notes"           #'+jg-bibtex-edit-notes
                                      "Tweet Entry"          #'+jg-bibtex-helm-tweet-action
                                      )
          :candidates 'helm-bibtex-candidates
          :filtered-candidate-transformer  '(+jg-bibtex-year-sort-transformer
                                             +jg-bibtex-helm-candidates-formatter
                                             helm-fuzzy-highlight-matches)
          :multimatch
          :fuzzy-match
          )
        )
  )
;;-- end sources