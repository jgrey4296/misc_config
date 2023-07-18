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
