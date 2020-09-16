;;json
(provide 'jgul-json)

(defun jg-tag-unify-layer/reformat-json-file (file)
  (assert (f-ext? file "json"))
  (with-temp-buffer
    (insert-file file)
    (json-mode-beautify)
    (write-file (format "%s_cleaned.json" (f-join (f-parent file)
                                                  (f-base file))))
    )
  )
(defun jg-tag-unify-layer/reformat-jsons ()
  "Beautify marked json files"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (seq-each 'jg-tag-unify-layer/reformat-json-file files)
    )
  )
