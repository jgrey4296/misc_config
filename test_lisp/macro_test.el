;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(defmacro inc (var)
  (list 'setq var (list '1+ var)))
(defvar a 0)

(defun test()
  (progn
    (print a)
    (inc a)
    (print a))
  )  

