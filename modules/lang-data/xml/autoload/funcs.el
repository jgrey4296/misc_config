;;; +funcs.el -*- lexical-binding: t; -*-

(defun +jg-xml-epub-manifest-generate ()
  (interactive)
  (let* ((marked (mapcar 'f-filename (dired-get-marked-files)))
         (indices (number-sequence 0 (length marked)))
         (type (ivy-read "Manifest Type: " '("application/xhtml+xml"
                                             "image/jpeg"
                                             "text/css"
                                             "<navpoint>"
                                             "<spine>"
                                             )))
         )
    (with-temp-buffer-window "*Generated-Manifest-Entries*" #'display-buffer-pop-up-window nil
      (cl-loop for ifile in (-zip-pair indices marked)
               do
               (cond ((s-equals? type "<navpoint>")

                      (princ (format "<navPoint class=\"chapter\" id=\"Chapter%s\" playOrder=\"%s\">\n" (car ifile) (car ifile)))
                      (princ "    <navLabel> <text>Title</text> </navLabel>\n")
                      (princ (format "    <content src=\"%s\"/>\n" (cdr ifile)))
                      (princ "</navPoint>\n")
                      )
                     ((s-equals? type "<spine>")
                      (princ (format "<itemref idref=\"%s\"/>\n" (f-no-ext (cdr ifile))))
                      )
                     (t
                      (princ (format "<item id=\"%s\" href=\"%s\" media-type=\"%s\"/>\n"
                                     (f-no-ext (cdr ifile))
                                     (cdr ifile)
                                     type))
                      )
                     )
               )
      )
    )
  )

(defun +jg-xml-validate ()
  (interactive)
  (let ((f-name (buffer-file-name))
        (xsd    (read-file-name "XSD Spec: "))
        )
    (save-buffer)
    (shell-command (format jg-xml-validate-command-string
                           (shell-quote-argument xsd)
                           (shell-quote-argument f-name)
                           ))
  )
)
