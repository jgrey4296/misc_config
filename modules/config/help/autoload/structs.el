;; structs.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-help-describe-class()
  " use cl-describe-struct  "
  (interactive)
  (ivy-read "Describe class: "
            obarray
            :predicate #'cl-find-class
            :action #'(lambda (x) (cl-describe-type (intern x)))
            )
  )
