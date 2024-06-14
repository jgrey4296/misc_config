;;; repl.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +emacs-lisp/open-repl (name)
  "Open the Emacs Lisp REPL (`ielm')."
  (interactive)
  (cl-assert (not (get-buffer name)))
  (ielm name)
  )

;;;###autoload
(defun +jg-lisp-eval (beg end)
  "Evaluate a region and print it to the echo area (if one line long), otherwise
to a pop up buffer."
  (+eval-display-results
   (string-trim-right
    (let ((buffer (generate-new-buffer " *+eval-output*"))
          (debug-on-error t))
      (unwind-protect
          (condition-case-unless-debug e
              (eval-region beg end buffer load-read-function)
            (with-current-buffer buffer
              (let ((pp-max-width nil))
                (require 'pp)
                (pp-buffer)
                (replace-regexp-in-string "\\\\n" "\n" (string-trim-left (buffer-string))))))
        (error (format "ERROR: %s" e)))
      (kill-buffer buffer))))
  (current-buffer))
