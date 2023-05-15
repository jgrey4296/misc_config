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

;;;###autoload
(defun +jg-bindings-calendar-insert-date ()
  " from https://emacs.stackexchange.com/questions/42529/insert-date-using-a-calendar "
  (interactive)
   "Capture the date at point, exit the Calendar, insert the date."
   (interactive)
   (seq-let (month day year) (save-match-data (calendar-cursor-to-date))
     (calendar-exit)
     (insert (format "%d-%02d-%02d" year month day)))
  )
