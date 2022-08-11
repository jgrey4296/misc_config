;;; +funcs.el -*- lexical-binding: t; -*-

(defun +jg-fold-toggle-auto-hide ()
  (interactive)
  (setq jg-fold-auto-hide-toggle (not jg-fold-auto-hide-toggle))
  (message "Auto Hide: %s" jg-fold-auto-hide-toggle))


(cl-defun +jg-fold-block-gen (&rest rst &key name (end nil) (re nil) (newlines nil) (comment comment-start))
  " Single point to build fold block markers
Atuo-recognizes the current major-mode's commment syntax
 "
  (let* ((comment-str (apply 'concat (make-list jg-fold-block-depth (s-trim comment))))
         (end-str (if end "end " nil))
         (name-pattern "\\(.+\\)")
         (name-form (s-concat end-str (or name name-pattern)))
         (full-pattern (format jg-fold-block-pattern comment-str name-form))
         )
    (cond ((and re newlines) (error "Fold Block Invalid Combined Args: :re and :newlines"))
          ((and newlines (not name)) (error ":newlines should also have :name"))
          (re       (s-concat "^[[:blank:]]*" full-pattern))
          (newlines (s-concat (if end "\n" "") full-pattern "\n"))
          (t full-pattern))))
