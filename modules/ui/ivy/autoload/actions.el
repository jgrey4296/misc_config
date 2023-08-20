;; actions.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-ivy--action-insert (x)
  " Ivy Insert, but with spaces between inserts if adding multipl "
  (insert (if (stringp x) (ivy--trim-grep-line-number x) x (car x))
          (if ivy-marked-candidates "\n" "")
          ))
