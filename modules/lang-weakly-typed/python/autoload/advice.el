;;; +advice.el -*- lexical-binding: t; -*-


;;;###autoload
(defun +jg-python-shell-calculate-command (&optional filepath)
  "Calculate the string used to execute the inferior Python process.
Adds in a few extra options like dev mode control,
a custom pycache location,
and adding extra pythonpath locations as the pre-args
"
  ;; `python-shell-make-comint' expects to be able to
  ;; `split-string-and-unquote' the result of this function.
  (combine-and-quote-strings
   (--remove (not it)
             (append jg-python-current-interpreter
                     (if jg-python-dev-mode jg-python-dev-cmd-args)
                     ;; (format jg-python-pycache-cmd (f-canonical jg-python-pycache-loc))
                     (ensure-list (or filepath
                                      python-shell-interpreter-path-args
                                      ))
                     )
             )
   )
  )

;;;###autoload
(defun +jg-python-conda-get-path-prefix (env-dir)
  "Get a platform-specific path string to utilize the conda env in ENV-DIR.
It's platform specific in that it uses the platform's native path separator."
  (let* ((conda-anaconda-home-tmp conda-anaconda-home)
         (conda-executable-path
          (concat (file-name-as-directory conda-anaconda-home-tmp)
                  (file-name-as-directory conda-env-executables-dir)
                  "conda"))
         (base-command jg-conda-activate-cmd)
         (command (format base-command env-dir))
         (result
          (with-output-to-string
            (with-current-buffer standard-output
              (unless (= 0 (process-file shell-file-name nil '(t nil) nil shell-command-switch command))
                (error (format "Error: executing command \"%s\" produced error code %d" command return-code)))
              ))))
    (s-trim result))
  )

;;;###autoload
(defun +python--init-completion-a (&rest args)
    "Call `pip-requirements-fetch-packages' first time completion is invoked."
    (unless pip-packages (pip-requirements-fetch-packages)))

;;;###autoload
(defun +python--inhibit-pip-requirements-fetch-packages-a (fn &rest args)
    "No-op `pip-requirements-fetch-packages', which can be expensive."
    (letf! ((#'pip-requirements-fetch-packages #'ignore))
      (apply fn args))
    )

;;;###autoload
(defun +jg-python-override-python-ts (fn &rest args)
  "Advice to apply :arround python-ts-mode,
to stop it interfering with `auto-mode-alist' and `interpreter-mode-alist'
"
  (let ((auto-mode-alist nil)
        (interpreter-mode-alist nil)
        )
    (apply fn args)
    )
)
