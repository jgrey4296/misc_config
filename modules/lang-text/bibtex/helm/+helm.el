;;; domain-specific/bibtex/+helm.el -*- lexical-binding: t; -*-

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
  (require 'helm)
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
