;;; util/bindings/+funcs.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-bindings-wk-filter-fn (binding)
  (not (string-match (rx (or "C-"
                             "C-M"
                             "M-"
                             ;; "s-"
                             ))
                     (car binding)))
  )

;;;###autoload
(defun +jg-binding-change-ext ()
  (interactive)
  (let* ((current (buffer-file-name))
        (curr-ext (f-ext current))
        (newext  (read-string (format "Extension %s -> ." curr-ext)))
        )
    (message "Converting %s -> %s" current (f-swap-ext current newext))
    (rename-file current (f-swap-ext current newext))
    )
  )
