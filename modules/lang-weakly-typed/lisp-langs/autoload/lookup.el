;;; lookup.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +racket-lookup-documentation (thing)
  "A `+lookup/documentation' handler for `racket-mode' and `racket-xp-mode'."
  (let ((buf (if racket-xp-mode
                 (racket-xp-describe thing)
               (racket-repl-describe thing))))
    (when buf
      (pop-to-buffer buf)
      t)))

;;;###autoload
(defun +racket-lookup-definition (_thing)
  "A `+lookup/definition' handler for `racket-mode' and `racket-xp-mode'."
  (call-interactively
   (if racket-xp-mode
       #'racket-xp-visit-definition
     #'racket-repl-visit-definition)))
