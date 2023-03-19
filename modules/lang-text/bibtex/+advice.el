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
  (let* ((loc default-directory)
         (build-loc (f-join loc "tex"))
         (bibfile   (file-name-nondirectory (buffer-file-name)))
         (bib-base  (file-name-sans-extension bibfile))
         (target    (f-join build-loc bib-base))
         (pdffile   (concat bib-base ".pdf"))
         (texfile   (concat bib-base ".tex")))
    (if (not (f-exists? build-loc))
        (f-mkdir build-loc))
    (with-temp-buffer
      (insert-file-contents (expand-file-name jg-bibtex-loc-export-bib-file))
      (goto-char (point-min))
      (re-search-forward "%target")
      (replace-match (f-join loc bibfile))
      (write-file (f-join build-loc texfile))
      )
    (let ((default-directory build-loc))
      (shell-command (concat "pdflatex " (shell-quote-argument texfile)))
      (shell-command (concat "bibtex "  (shell-quote-argument bib-base)))
      (shell-command (concat "pdflatex " (shell-quote-argument texfile)))
      (shell-command (concat "pdflatex " (shell-quote-argument texfile)))
      (shell-command (concat jg-bibtex-open-pdf-cmd pdffile))
      )
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
    (if (setq found (bibtex-search-forward-field (concat field "\\b") t))
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

(define-advice bibtex-autokey-get-field (:around (fn &rest args)
                                         +jg-bibtex-autokey-field-expand)
  (if-let* ((first-arg (if (listp (car args)) (car args) (list (car args))))
            (matches (-any? (-partial #'s-matches? "file[[:digit:]]*") first-arg))
            (result (apply fn args))
            (not-empty (not (s-equals? "" result)))
            )
      (+jg-bibtex-file-expand result)
    (apply fn args)
    )
  )

;;; +advice.el ends here
