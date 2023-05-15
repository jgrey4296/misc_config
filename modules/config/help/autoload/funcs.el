;;; funcs.el -*- lexical-binding: t; -*-

(defvar jg-help-local-var-skip-regexp (rx (or "-map"
                                              "keymap"
                                              "display-table"
                                              "imenu-generic-expression"
                                              "font-lock-keywords"))
  )

;;;###autoload
(defun +jg-help-list-buffer-locals ()
  (interactive)
  (let ((vars (buffer-local-variables))
        (buf (buffer-name (current-buffer)))
        )
    (with-temp-buffer-window (format "*Buffer Locals: %s" buf)
        'display-buffer-pop-up-window
        (lambda (wind val) (with-selected-window wind
                        (emacs-lisp-mode))
          val)
      (cl-loop for x in vars do
               (if (or (string-match jg-help-local-var-skip-regexp
                                     (symbol-name (car x)))
                        (< 40 (length (format "%s" (cdr x)))))
                   (princ (format "(%s : Skipped)" (car x)))
                 (princ x))
               (princ "\n")
               )
      )
    )
  )
