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
    (shell-command (concat "pdflatex " target))
    (shell-command (concat "bibtex " target))
    (shell-command (concat "pdflatex " target))
    (shell-command (concat "pdflatex " target))
    (org-open-file pdffile)
    )
  )



;;; +advice.el ends here