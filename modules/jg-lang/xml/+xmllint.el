;;; +xmllint.el -*- lexical-binding: t; -*-

(defun +jg-xml-run-xpath (query)
  " Run an xpath query on the current buffer / file
Storing and displaying results in a buffer
  "
  (interactive "sQuery: ")
  (let* ((current (current-buffer))
         (fname (buffer-file-name current))
         (default-directory (f-parent fname))
         (result (shell-command-to-string (format jg-xml-xpath-command-string query (f-filename fname))))
         )
    (with-current-buffer (get-buffer-create jg-xml-xpath-results-buffer-name)
      (insert result)
      )
    (pop-to-buffer jg-xml-xpath-results-buffer-name)
    )
  )

(defun +xml/open-repl ()
  " Open an xml lint shell comint on the current buffer / file "
  (interactive)
  (let* ((fname (buffer-file-name (current-buffer)))
         (default-directory (f-parent fname))
         (comint-buff (make-comint-in-buffer "xmllint"
                                             jg-xml-xmllint-shell-buffer-name
                                             "xmllint"
                                             nil
                                             "--shell"
                                             (f-filename fname)
                                             ))
          )
    (with-current-buffer comint-buff
      (nxml-mode)
      )
    (pop-to-buffer comint-buff)

    )
  )

(set-repl-handler! 'nxml-mode #'+xml/open-repl)
