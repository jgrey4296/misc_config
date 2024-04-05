;;; repl.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +racket/open-repl ()
  "Open the Racket REPL."
  (interactive)
  (pop-to-buffer
   (or (get-buffer "*Racket REPL*")
       (progn (racket-run-and-switch-to-repl)
              (let ((buf (get-buffer "*Racket REPL*")))
                (bury-buffer buf)
                buf)))))

;;;###autoload
(defun +emacs-lisp/open-repl ()
  "Open the Emacs Lisp REPL (`ielm')."
  (interactive)
  (pop-to-buffer
   (or (get-buffer "*ielm*")
       (progn (ielm)
              (let ((buf (get-buffer "*ielm*")))
                (bury-buffer buf)
                buf)))))

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
