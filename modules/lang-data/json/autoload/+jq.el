;;; +jq.el -*- lexical-binding: t; -*-

;; TODO make async
(defvar jg-text-jq-cmd "jq")

;;;###autoload
(defun +jg-json-jq-format ()
  (interactive)
  (let ((marked (dired-get-marked-files)))
    (cl-loop for file in marked
             do
             (with-temp-file (f-join (f-parent file)
                                     (concat "fmt-" (f-filename file)))
               (call-process jg-text-jq-cmd nil t nil "." file)
               )
             )
    )
  )

;;;###autoload
(defun +jg-json-jq-expr()
  (interactive)
  (let ((marked (dired-get-marked-files))
        (expr (read-string "JQ Expr: " "."))
        )
    (cl-loop for file in marked
             do
             (with-temp-file (f-join (f-parent file)
                                     (concat "fmt-" (f-filename file)))
               (call-process jg-text-jq-cmd nil t nil expr file)
               )
             )
    )
  )

;;;###autoload
(defun +jg-dired-reformat-json-file (file)
  (cl-assert (f-ext? file "json"))
  (with-temp-buffer
    (insert-file file)
    (json-mode-beautify)
    (write-file (format "%s_cleaned.json" (f-join (f-parent file)
                                                  (f-base file))))
    )
  )

;;;###autoload
(defun +jg-dired-reformat-jsons ()
  "Beautify marked json files"
  (interactive)
  (let ((files (dired-get-marked-files)))
    (seq-each 'jg-tag-reformat-json-file files)
    )
  )
