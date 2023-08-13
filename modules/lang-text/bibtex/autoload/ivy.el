;;; ivy.el -*- lexical-binding: t; -*-
(require 'ivy)
(require 'ivy-bibtex)

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
              :caller 'jg-ivy-bibtex
              :history 'ivy-bibtex-history
              :action ivy-bibtex-default-action
              :multi-action ivy-bibtex-default-multi-action
              :keymap (when ivy-bibtex-use-extra-keymap ivy-bibtex-extra-keymap)))
  )

(ivy-set-display-transformer 'jg-ivy-bibtex 'ivy-bibtex-display-transformer)

(ivy-set-actions 'jg-ivy-bibtex
 '(("p" ivy-bibtex-open-pdf                                  "Open PDF file (if present)" ivy-bibtex-open-pdf)
   ("u" ivy-bibtex-open-url-or-doi                           "Open URL or DOI in browser" ivy-bibtex-open-url-or-doi)
   ("c" ivy-bibtex-insert-citation                           "Insert citation"            ivy-bibtex-insert-citation)
   ("r" ivy-bibtex-insert-reference                          "Insert reference"           ivy-bibtex-insert-reference)
   ("k" ivy-bibtex-insert-key                                "Insert BibTeX key"          ivy-bibtex-insert-key)
   ("b" ivy-bibtex-insert-bibtex                             "Insert BibTeX entry"        ivy-bibtex-insert-bibtex)
   ("a" ivy-bibtex-add-PDF-attachment                        "Attach PDF to email"        ivy-bibtex-add-PDF-attachment)
   ("e" ivy-bibtex-edit-notes                                "Edit notes"                 ivy-bibtex-edit-notes)
   ("s" ivy-bibtex-show-entry                                "Show entry"                 ivy-bibtex-show-entry)
   ("l" ivy-bibtex-add-pdf-to-library                        "Add PDF to library"         ivy-bibtex-add-pdf-to-library)
   ("f" (lambda (_candidate) (ivy-bibtex-fallback ivy-text)) "Fallback options")
   )
 )
