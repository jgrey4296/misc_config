
(defun myDoListTest ()
  (interactive)
  (dolist (x `(1 2 3 4 5))
    (insert "blah")))
