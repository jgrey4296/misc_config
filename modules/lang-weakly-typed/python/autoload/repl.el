;;; lang/python/autoload/python.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +python-executable-find (exe)
  "Resolve the path to the EXE executable.
Tries to be aware of your active conda/pipenv/virtualenv environment, before
falling back on searching your PATH."
  (if (file-name-absolute-p exe)
      (and (file-executable-p exe)
           exe)
    (let ((exe-root (format "bin/%s" exe)))
      (cond ((when python-shell-virtualenv-root
               (let ((bin (expand-file-name exe-root python-shell-virtualenv-root)))
                 (if (file-exists-p bin) bin))))
            ((when (require 'conda nil t)
               (let ((bin (expand-file-name (concat conda-env-current-name "/" exe-root)
                                            (conda-env-default-location))))
                 (if (file-executable-p bin) bin))))
            ((when-let (bin (projectile-locate-dominating-file default-directory "bin/python"))
               (setq-local doom-modeline-python-executable (expand-file-name "bin/python" bin))))
            ((executable-find exe))))))

;;;###autoload
(defun +jg-python/open-repl ()
  "Open the Python REPL."
  (interactive)
  (require 'python)
  (unless python-shell--interpreter
    (user-error "`python-shell-interpreter' isn't set"))

  (unless env-handling-state
    (env-handling-go!))

  ;; (puthash (cons 'inferior-python-mode default-directory) new-buffer +eval-repl-buffers)
  ;; (puthash (cons 'python-mode default-directory) new-buffer +eval-repl-buffers)

  (pop-to-buffer
   (process-buffer
    (let ((dedicated (bound-and-true-p python-shell-dedicated))
          (default-directory (projectile-project-root))
          )
      (run-python nil dedicated t)
      )
    )
   )
  )

;;;###autoload
(defun +jg-python/open-ipython-repl ()
  "Open the iPython REPL."
  (interactive)
  (require 'python)
  (unless python-shell--interpreter
    (user-error "`python-shell-interpreter' isn't set"))

  (unless env-handling-state
    (env-handling-go!))

  (let ((python-shell-interpreter (or (executable-find (car +python-ipython-command)) "ipython"))
        (python-shell-interpreter-args (string-join (cdr +python-ipython-command) " "))
        (dedicated (bound-and-true-p python-shell-dedicated))
        (default-directory (projectile-project-root))
        )
    (pop-to-buffer
     (process-buffer
      (run-python nil dedicated t)
      )
     )
    )
)

;;;###autoload
(defun +python/open-file-repl ()
  (interactive)
  (cl-assert (eq (with-current-buffer (current-buffer) major-mode) 'python-mode))
  (unless python-shell-interpreter

    (user-error "`python-shell-interpreter' isn't set"))

  (let* ((default-directory (doom-project-root))
         (cmd (python-shell-calculate-command (buffer-file-name)))
         (new-buffer (process-buffer
                      (run-python cmd nil t))))
    (puthash (cons 'inferior-python-mode default-directory) new-buffer +eval-repl-buffers)
    (puthash (cons 'python-mode default-directory) new-buffer +eval-repl-buffers)
    new-buffer
    )
  )

;;;###autoload
(defun +python/optimize-imports ()
  "organize imports"
  (interactive)
  (pyimport-remove-unused)
  (py-isort-buffer))
