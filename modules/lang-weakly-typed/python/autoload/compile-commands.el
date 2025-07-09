;;; compile-commands.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-python-get-commands (&optional dir)
  (interactive)
  (-when-let* ((root (projectile-project-root dir))
               (project (f-join root "pyproject.toml"))
               (project-exists (f-exists? project))
               (curr-file (buffer-file-name))
               )
    (-reject #'null (+jg-eval--pair-cmds
     ;; python
     '("shell" "echo 'shell: ' $0")
     '("py versions" "mamba info; python -V -V; pip --version ; pytest --version; echo 'Env ' $CONDA_DEFAULT_ENV; sphinx-build --version")
     ;; pip
     '("pip list" "pip list")
     ;; conda/mamba
     '("mamba envs" "mamba env list")
     '("mamba list" "mamba list")
     '("mamba info" "mamba info")
     ;; uv
     '("uv sync" "uv sync --all-extras")
     '("uv deps" "uv tree")
     ;; pytest
     '("pytest" "pytest")
     '("pytest version" "pytest --version")
     ;; Sphinx
     '("sphinx py docs" "doot docs::build")
     (when (f-ext? curr-file "rst")
        `("docfile" ,(format "doot docs::build.file %s" curr-file)))
     ))
    )
  )

;;;###autoload
(defun +jg-python-solo-file-run (&optional dir)
  (interactive)
  (-when-let* ((filename (buffer-file-name))
               (is-py (f-ext? filename "py"))
               )
    (+jg-eval--pair-cmds
     `("run-py"         ,(format "python -X dev %s" filename)    :interactive)
     `("run-py-verbose" ,(format "python -X dev -i -v %s" filename) :interactive)
     `("run-ipy"        ,(format "ipython -i %s" filename)          :interactive)
      )
  )
)
