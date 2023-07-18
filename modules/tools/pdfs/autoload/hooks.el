;;; hooks.el -*- lexical-binding: t; -*-


;;;###autoload
(defun +org-open-legacy-pdf-links-fn (link)
  "Open pdftools:* and pdfviews:* links as if they were pdf:* links."
  (let ((regexp "^pdf\\(?:tools\\|view\\):"))
    (when (string-match-p regexp link)
      (org-pdftools-open (replace-regexp-in-string regexp "" link))
      t)))
