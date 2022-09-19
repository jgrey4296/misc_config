;;; +xmllint.el -*- lexical-binding: t; -*-

(defun +jg-xml-run-xidel (query)
  " Run an xpath query on the current buffer / file
Storing and displaying results in a buffer
uses xidel, outputs as xml
  "
  (interactive "sXML Query: ")
  (let* ((current (current-buffer))
         (fname (buffer-file-name current))
         (default-directory (f-parent fname))
         (result (shell-command-to-string (format jg-xml-xidel-command-string
                                                  query
                                                  (shell-quote-argument  (f-filename fname)))))
         )
    (with-current-buffer (get-buffer-create jg-xml-xpath-results-buffer-name)
      (erase-buffer)
      (insert result)
      (nxml-mode)
      )
    (display-buffer  jg-xml-xpath-results-buffer-name)
    )
  )

(defun +jg-xml-dired-run-xidel (query)
  (interactive "sXML Query: ")
  (let* ((marked (dired-get-marked-files))
         (target-dir (read-directory-name "Output Directory: "))
         (prefix (read-string "Prefix filename with: "))
         (results (cl-loop for fname in marked
                           do
                           (let ((result (shell-command-to-string (format jg-xml-xidel-command-string
                                                                          query
                                                                          (shell-quote-argument fname))))
                                 (new-fname (f-join target-dir (concat prefix (f-swap-ext (f-filename fname) "xml"))))
                                 )
                             (append-to-file result nil new-fname)
                             ;; result

                             )
                           ))
         )
    1
    ;; (with-current-buffer (get-buffer-create jg-xml-xpath-results-buffer-name)
    ;;   (erase-buffer)
    ;;   (mapc #'insert results)
    ;;   (nxml-mode)
    ;;   )
    ;; (display-buffer  jg-xml-xpath-results-buffer-name)
    ;; )
  )

  )
