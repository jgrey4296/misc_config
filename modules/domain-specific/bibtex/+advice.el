;;; +advice.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <johngrey4296 at gmail.com>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: March 22, 2022
;; Modified: March 22, 2022
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/johngrey/+advice
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(define-advice org-ref-build-full-bibliography (:override () +jg-build-bibliography)
  "Build pdf of all bibtex entries, and open it."
  (interactive)
  (let* ((bibfile (file-name-nondirectory (buffer-file-name)))
         (bib-base (file-name-sans-extension bibfile))
         (pdffile (concat bib-base ".pdf"))
         (target bib-base)
         (texfile (concat target ".tex")))
    (with-temp-buffer
      (insert-file-contents jg-bibtex-loc-export-bib-file)
      (goto-char (point-min))
      (re-search-forward "%target")
      (replace-match (concat default-directory bibfile))
      (write-file  texfile)
      )
    (shell-command (concat "pdflatex " (shell-quote-argument target)))
    (shell-command (concat "bibtex "   (shell-quote-argument target)))
    (shell-command (concat "pdflatex " (shell-quote-argument target)))
    (shell-command (concat "pdflatex " (shell-quote-argument target)))
    (org-open-file pdffile)
    )
  )

(define-advice org-ref-version (:around (f)
                                +jg-org-ref-version-override)
  (let ((kill-ring nil))
    (funcall f)
    )
  )

(define-advice bibtex-set-field (:override (field value &optional nodelim)
                                 +jg-bibtex-set-field)
  "Set FIELD to VALUE in bibtex file.  create field if it does not exist.
Optional argument NODELIM ignored to fit `bibtex-make-field` signature
Modified to avoid duplicate comma insertion. "
  (interactive "sfield: \nsvalue: ")
  (bibtex-beginning-of-entry)
  (let ((found))
    (if (setq found (bibtex-search-forward-field field t))
        ;; we found a field
        (progn
          (goto-char (car (cdr found)))
          (when value
            (bibtex-kill-field)
            (bibtex-make-field field nil nil nil)
            (backward-char)
            (insert value)))
      ;; make a new field
      (bibtex-beginning-of-entry)
      (forward-line) (beginning-of-line)
      ;; (bibtex-next-field nil)
      ;; (forward-char)
      (bibtex-make-field field t nil nil)
      (backward-char)
      (insert value))))

;;; +advice.el ends here
