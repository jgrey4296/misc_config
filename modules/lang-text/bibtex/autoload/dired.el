;;; +dired.elj -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <johngrey4296 at gmail.com>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: March 22, 2022
;; Modified: March 22, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/johngrey/+dired
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;;;###autoload
(defun +jg-bibtex-dired-compile ()
  (interactive)
  (let ((marked (-filter (lambda (x) (s-equals? "bib" (f-ext x))) (dired-get-marked-files)))
        (temp (f-join (projectile-project-root) ".temp"))
        )
    (mkdir (f-join temp "tex") t)
    (cl-loop for fname in marked
          do
          (let* ((bibfile (file-name-nondirectory fname))
                 (bib-base (file-name-sans-extension bibfile))
                 (target bib-base)
                 (texfile (f-join temp "tex" (concat target ".tex"))))
            (with-temp-buffer
              (insert-file-contents jg-bibtex-loc-export-bib-file)
              (goto-char (point-min))
              (re-search-forward "%title")
              (replace-match (f-filename (f-no-ext bibfile)))
              (re-search-forward "%target")
              (replace-match (concat default-directory bibfile))
              (write-file texfile)
              )
            )
          )
    )
  )

;;;###autoload
(defun +jg-bibtex-dired-compile-run ()
  (interactive)
  (let ((marked (-filter (lambda (x) (s-equals? "tex" (f-ext x))) (dired-get-marked-files))))
    (cl-loop for fname in marked
          do
          (let* ((bibfile (file-name-nondirectory fname))
                 (bib-base (file-name-sans-extension bibfile))
                 (target bib-base)
                 (texfile (concat target ".tex")))
            (shell-command (concat "pdflatex " (shell-quote-argument target)))
            (shell-command (concat "bibtex "   (shell-quote-argument target)))
            (shell-command (concat "pdflatex " (shell-quote-argument target)))
            (shell-command (concat "pdflatex " (shell-quote-argument target)))
            )
          )
    )
  )


;;; +dired.el ends here
