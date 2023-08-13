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
(defun +jg-bibtex-dired-generate-tex ()
  " Generate a tex file for the bibtex file "
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
              (goto-char (point-min))
              (while (re-search-forward "%target" nil t)
                (replace-match (concat default-directory bibfile))
                )
              (write-file texfile)
              )
            )
          )
    )
  )

;;;###autoload
(defun +jg-bibtex-dired-compile-run ()
  " Run Pdflatex -> bibtex -> pdflatex*2 on each marked tex file"
  (interactive)
  (let ((marked (-filter (lambda (x) (s-equals? "tex" (f-ext x))) (dired-get-marked-files)))
        errors
        success
        )
    (cl-loop for fname in marked
             do
             (let* ((bibfile (file-name-nondirectory fname))
                    (bib-base (file-name-sans-extension bibfile))
                    (target bib-base)
                    (texfile (concat target ".tex"))
                    )
               (condition-case err
                   (progn
                     (call-process "pdflatex" nil nil nil target)
                     (unless (f-exists? (format "%s.pdf" target))
                       (user-error target))
                     (call-process "bibtex"   nil nil nil target)
                     (unless (f-exists? (format "%s.bbl" target))
                       (user-error target))
                     (call-process "pdflatex" nil nil nil target)
                     (call-process "pdflatex" nil nil nil target)
                     (unless (f-exists? (format "%s.pdf" target))
                       (user-error target))

                     (add-to-list 'success target)
                     )
                 (t (add-to-list 'errors (cadr err)))
                 )
               )
             )

    (with-temp-buffer-window "*Bibtex Failures*" #'display-buffer nil
      (princ "* Succeeded:\n")
      (princ (string-join success "\n"))
      (princ "\n\n")
      (princ "* Failed:\n")
      (princ (string-join errors "\n"))
      )
    )
  )

;;;###autoload
(defun +jg-bibtex-dired-check-pdfs ()
  (interactive)
  (let* ((marked (-filter (-rpartial #'f-ext? "bib") (dired-get-marked-files)))
         (pdf-dir (f-join (projectile-project-root) ".temp" "pdfs"))
         (checked (mapcar (lambda (x)
                            (let ((pdf (f-join pdf-dir (f-swap-ext (f-filename x) "pdf"))))
                              (list (f-exists? pdf) pdf x )))
                          marked))
         )
    (with-temp-buffer-window "*Missing bib pdfs*" #'display-buffer nil
      (princ (format "Missing bib pdfs (checked %s):\n" (length marked)))
      (mapc (lambda (x) (unless (car x)
                          (princ (format "%s\n" (cdr x)))))
            checked
            )
      )
    )
  )



;;; +dired.el ends here
