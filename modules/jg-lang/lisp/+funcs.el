;;; jg-list/+funcs.el --- summary -*- lexical-binding: t -*-

;; Simple Functions to feed into sort-subr
(defun +jg-lisp-key-start ()
  (re-search-forward "(defun " nil t)
  (symbol-at-point))
(defun +jg-lisp-next-rec-end-func ()
  (evil-forward-end 'evil-defun))
(defun +jg-lisp-next-rec-func ()
  (evil-forward-beginning 'evil-defun))
(defun +jg-lisp-sort-defuns ()
  " A Lisp buffer sorting function "
  (interactive)
  (goto-char (point-min))
  (+jg-lisp-next-rec-func)
  (sort-subr nil
             #'+jg-lisp-next-rec-func
             #'+jg-lisp-next-rec-end-func
             #'+jg-lisp-key-start)
  (goto-char (point-min))
  )
(defun +jg-lisp-eval (beg end)
  "Evaluate a region and print it to the echo area (if one line long), otherwise
to a pop up buffer."
  (+eval-display-results
   (string-trim-right
    (let ((buffer (generate-new-buffer " *+eval-output*"))
          (debug-on-error t))
      (unwind-protect
          (condition-case-unless-debug e
              (let ((doom--current-module (ignore-errors (doom-module-from-path buffer-file-name))))
                (eval-region beg end buffer load-read-function)
                (with-current-buffer buffer
                  (let ((pp-max-width nil))
                    (require 'pp)
                    (pp-buffer)
                    (replace-regexp-in-string "\\\\n" "\n" (string-trim-left (buffer-string))))))
            (error (format "ERROR: %s" e)))
        (kill-buffer buffer))))
   (current-buffer)))
