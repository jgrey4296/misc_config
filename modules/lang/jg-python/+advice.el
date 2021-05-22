;;; lang/+advice.el -*- lexical-binding: t; -*-
;;;

(defun find-pyvenv (start)
  " Given a starting directory, look in parent dirs
until a .venv file is found.

return (dir-of-venv env-name) or nil
"
  (let* ((cwd default-directory)
         env-name
         )
    ;; Look for venv file
    (while (and cwd (not (f-exists? (f-join cwd ".venv"))))
      (setq cwd (f-parent cwd))
      )

    ;; Get the environment name
    (if (and cwd (f-exists? (f-join cwd ".venv")))
        (with-temp-buffer
          (insert-file-contents (f-join cwd ".venv"))
          (goto-char (point-min))
          (setq env-name (buffer-substring-no-properties (point-min) (line-end-position)))
          )
     )

    ;; return nil or (venv-dir env-name)
    (if env-name
        (list cwd env-name)
      nil)
    )
  )

(define-advice pyvenv-activate (:around (orig &optional already-found)
                                +jg-python-venv-automate-advice)
  " Auto-detect python environment "
  (interactive)
  (message "pyvenv-activate advice: %s" already-found)
  (cl-destructuring-bind (venv-dir env-name)
      (or already-found
          (find-pyvenv default-directory)
          (list default-directory
                (read-directory-name "Activate venv: " nil nil nil
                                     pyvenv-default-virtual-env-name)))
    (message "Activating Environment: %s in %s" env-name venv-dir)
    (funcall orig env-name))
  )


(define-advice +eval-open-repl (:around (orig prompt-p &optional displayfn)
                                +jg-python-repl-advice)
  " Auto-detect python repl and activate environment if necessary "

  ;; look for a venv
  (if-let ((found-pyvenv (find-pyvenv default-directory)))
        ;activate environment, start python repl
      (let ((default-directory (car found-pyvenv))
            (env-name (cadr found-pyvenv)))
        (message "Activating then py: %s" found-pyvenv)
        (pyvenv-activate found-pyvenv)
        (+python/open-repl))
    ;; start repl as normal
    (progn
      (message "open repl normally: %s %s" prompt-p displayfn)
      (funcall-interactively orig prompt-p displayfn)
      )
    )
  )
