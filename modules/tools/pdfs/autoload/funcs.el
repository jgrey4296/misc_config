;;; funcs.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org--pdftools-link-handler (fn &rest args)
      "Produces a link handler for org-pdftools that suppresses missing-epdfinfo errors whenever storing or exporting links."
      ;; hack Fixes an issue where org-pdftools link handlers will throw a
      ;;      'pdf-info-epdfinfo-program is not executable' error whenever any
      ;;      link is stored or exported (whether or not they're a pdf link). This
      ;;      error gimps org until `pdf-tools-install' is run, but this is poor
      ;;      UX, so we suppress it.
      (lambda (&rest args)
        (and (ignore-errors (require 'org-pdftools nil t))
             (file-executable-p pdf-info-epdfinfo-program)
             (apply fn args))))


(defvar jg-pdf-to-text-call "pdftotext")

;;;###autoload
(defun +jg-pdf-to-text ()
  (interactive)
  (let ((files (-filter #'f-file? (dired-get-marked-files)))
        )
    (cl-loop for file in files
             do
             (let* ((pages (pcase (read-string "Pages: ")
                             ((and x (pred (s-contains? "-" x)))
                              (s-split "-" x))
                             (x (list x x))
                             ))
                    (call (list jg-pdf-to-text-call
                                "-f" (car pages)
                                "-l" (cadr pages)
                                file
                                (f-swap-ext file "txt")
                                )
                          )
                    )
               (make-process :name (format "pdftotext-%s" (f-base file))
                             :buffer nil
                             :command call)

               )
             )
    )
  )

;;;###autoload
(defun +jg-pdf-linearize ()
  "run qpdf linearization"
  (interactive)
  (let ((files (-filter #'f-file? (dired-get-marked-files)))
        )
    (cl-loop for file in files
             do
             (let* ((call (list "qpdf"
                                file
                                "--linearize"
                                (f-join (f-dirname file)
                                        (format "%s-linear.pdf" (f-base file))
                                        )
                                )
                          )
                    )
               (make-process :name (format "qpdf linearization: %s" (f-base file))
                             :buffer nil
                             :command call)
               )
             )
    )
  )
