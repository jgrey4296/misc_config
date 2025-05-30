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
(defun +jg-python/open-repl (name)
  "Open the Python REPL."
  (interactive)
  (require 'python)
  (unless python-shell--interpreter (user-error "`python-shell-interpreter' isn't set"))

  (let ((cmd (+jg-python-shell-calculate-command))
        (dedicated (bound-and-true-p python-shell-dedicated))
        (default-directory (projectile-project-root))
        )
    (process-buffer (run-python cmd dedicated t))
    )
  )

;;;###autoload
(defun +python/open-file-repl ()
  (interactive)
  (error "TODO")
  (cl-assert (eq (with-current-buffer (current-buffer) major-mode) 'python-mode))
  (unless python-shell-interpreter
    (user-error "`python-shell-interpreter' isn't set"))

  (let* ((default-directory (projectile-project-root))
         (cmd (python-shell-calculate-command (buffer-file-name)))
         (new-buffer (process-buffer
                      (run-python cmd nil t))))
    (puthash (cons 'inferior-python-mode default-directory) new-buffer +eval-repl-buffers)
    (puthash (cons 'python-mode default-directory) new-buffer +eval-repl-buffers)
    new-buffer
    )
  )

;;;###autoload
(defun +jg-python-select-repl ()
    (interactive)
    (pcase (ivy-read "Set Python Interpreter: " '(default ipython))
      ("default"
       (setq jg-python-current-interpreter jg-python-stock-repl))
      ("ipython"
       (setq jg-python-current-interpreter +python-ipython-command))
      )
    )
