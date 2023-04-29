;;; registers.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-text-clear-all ()
  (interactive)
  (message "Clearing Registers")
  (setq register-alist nil)
  )

;;;###autoload
(defun +jg-text-clear-buffer ()
  " Utility to clear a buffer
    from https://stackoverflow.com/questions/24565068/ "
  (interactive)
  (let ((inhibit-read-only t)) (erase-buffer))
  )

;;;###autoload
(defun +jg-text-split-on-leading-char (char-no width dist)
  " Loop through the buffer, splitting lines if the substring has a greater levenstein distance from the previous line "
  (interactive "nChar Index: \nnWidth: \nnDistance: \n")
  (message "Char: %s, Dist: %s" char-no dist)
  (goto-char (point-min))
  (let ((get-line (lambda () (let ((line (s-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
                               (if (string-empty-p line)
                                   ""
                                 (substring line char-no (+ char-no width))))))
        last-line curr-line)
    (while (< (point) (point-max))
      (setq last-line (funcall get-line))
      (forward-line)
      (setq curr-line (funcall get-line))
      (if (< dist (string-distance last-line curr-line))
          (insert "\n")
        )
      )
    )
  )

;;;###autoload
(defun +jg-text-run-whitespace-cleanup ()
  "Operator to run cleaning hooks"
  (interactive)
  (let ((inhibit-read-only t))
    (cl-loop for hook in jg-text-whitespace-clean-hook
             when (functionp hook)
             do
             (save-excursion
               (goto-char (point-min))
               (apply hook nil)
               )
             )
  )
)
