;;; funcs.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-calendar-insert-date ()
  " from https://emacs.stackexchange.com/questions/42529/insert-date-using-a-calendar "
  (interactive)
   "Capture the date at point, exit the Calendar, insert the date."
   (interactive)
   (seq-let (month day year) (save-match-data (calendar-cursor-to-date))
     (calendar-exit)
     (insert (format "%d-%02d-%02d" year month day)))
  )
