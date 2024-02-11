;;; funcs.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

(defvar jg-bibtex-form-last-crossref "")
(defvar jg-bibtex-form-last-year     "")

;;;###autoload
(defun +jg-bibtex-entry-form ()
  (interactive)
  (let ((type (completing-read "Entry Type: " bibtex-entry-alist nil t nil 'bibtex-entry-type-history)))
    (bibtex-entry type)
    (save-excursion
      (pcase type
        ("Book"
         (+jg-bibtex-edit-field "title")
         (+jg-bibtex-edit-field "author")
         (+jg-bibtex-edit-field "year")
         (+jg-bibtex-edit-field "publisher")
         )
        ("Article"
         (+jg-bibtex-edit-field "title")
         (+jg-bibtex-edit-field "author")
         (+jg-bibtex-edit-field "year")
         (+jg-bibtex-edit-field "journal")
         )
        ("Online"
         (+jg-bibtex-edit-field "title")
         (+jg-bibtex-edit-field "author")
         (+jg-bibtex-edit-field "year")
         (+jg-bibtex-edit-field "url")
         )
        ("InProceedings"
         (+jg-bibtex-edit-field "title")
         (+jg-bibtex-edit-field "author")
         (setq jg-bibtex-form-last-crossref (+jg-bibtex-edit-field "crossref" jg-bibtex-form-last-crossref))
         (setq jg-bibtex-form-last-year (+jg-bibtex-edit-field "year" jg-bibtex-form-last-year))
         )
        )
      )
    )
  )

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    February 07, 2024
;; Modified:   February 07, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; funcs.el ends here
