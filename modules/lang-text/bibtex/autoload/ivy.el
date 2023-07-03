;;; ivy.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-bibtex-ivy ()
  (interactive)
  (when (null jg-bibtex-helm-candidates)
    (message "Generating Candidates")
    (setq jg-bibtex-helm-candidates
          (mapcar '+jg-bibtex-process-candidates (bibtex-completion-candidates)))

    )
  (let* ((candidates jg-bibtex-helm-candidates)
         (key (bibtex-completion-key-at-point))
         (preselect (and key
                         (cl-position-if (lambda (cand)
                                           (member (cons "=key=" key)
                                                   (cdr cand)))
                                         candidates))))
    (ivy-read (format "BibTeX entries%s: " (if local-bib " (local)" ""))
              candidates
              :preselect preselect
              :caller 'ivy-bibtex
              :history 'ivy-bibtex-history
              :action ivy-bibtex-default-action
              :multi-action ivy-bibtex-default-multi-action
              :keymap (when ivy-bibtex-use-extra-keymap ivy-bibtex-extra-keymap)))
  )
