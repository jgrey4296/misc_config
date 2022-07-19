;;; +operators.el -*- lexical-binding: t; -*-

(evil-define-operator +jg-wrap-fold-block (beg end count &optional name)
  " Operator to easily create fold blocks "
  :type block
  :keep-visual t
  (interactive "<R>" (list (read-string "Block Name: ")))
  ;; From bottom to top to not change interfere with positions
  ;; add end fold block
  (goto-char end)
  (end-of-line)
  (insert (+jg-fold-block-gen :name name :end t :newlines t))
  ;; and start fold block
  (goto-char beg)
  (beginning-of-line)
  (insert (+jg-fold-block-gen :name name :newlines t))
  )

(cl-defun +jg-fold-block-gen (&rest rst &key name (end nil) (re nil) (newlines nil) (comment comment-start))
  " Single point to build fold block markers
Atuo-recognizes the current major-mode's commment syntax
 "
  (let* ((comment-str (apply 'concat (make-list jg-fold-block-depth (s-trim comment))))
         (end-str (if end "end " nil))
         (name-pattern "\\(.+\\)")
         (name-form (s-concat end-str (if name name name-pattern)))
         (full-pattern (format jg-fold-block-pattern comment-str name-form))
         )
    (cond ((and re newlines) (error "Fold Block Invalid Combined Args: :re and :newlines"))
          ((and newlines (not name)) (error ":newlines should also have :name"))
          (re       (s-concat "^" full-pattern))
          (newlines (s-concat (if end "\n" "") full-pattern "\n"))
          (t full-pattern))))
