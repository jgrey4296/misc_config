;;; +jq.el -*- lexical-binding: t; -*-

(defvar jg-text-jq-cmd "jq")

;;;###autoload
(defun +jg-text-jq-format ()
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
(defun +jg-text-jq-expr()
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
