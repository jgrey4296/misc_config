;;; compile-commands.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-python-get-commands (&optional dir)
  (interactive)
  (-when-let* ((root (projectile-project-root dir))
               (project (f-join root "pyproject.toml"))
               (project-exists (f-exists? project))
               )
    (+jg-projects-pair-cmds
     ;; python
     '("shell" "echo 'shell: ' $0")
     '("versions" "mamba info; python -V -V; pip --version ; pytest --version; echo 'Env ' $CONDA_DEFAULT_ENV; sphinx-build --version")
     ;; pip
     '("pip list" "pip list")
     ;; conda/mamba
     '("envs" "mamba env list")
     '("mamba list" "mamba list")
     '("mamba info" "mamba info")
     ;; pytest
     '("test" "pytest")
     '("pytest version" "pytest --version")
     ;; Sphinx
     '("docs" "sphinx-build -b html ./docs ./.temp/docs")
     )
    )
  )

;;;###autoload
(defun +jg-python-solo-file-run (&optional dir)
  (interactive)
  (-when-let (is-py (f-ext? (buffer-file-name) "py"))
    (+jg-projects-pair-cmds
     `("run-py"         ,(format "python -X dev -i %s" (buffer-file-name))    :interactive)
     `("run-py-verbose" ,(format "python -X dev -i -v %s" (buffer-file-name)) :interactive)
     `("run-ipy"        ,(format "ipython -X %s" (buffer-file-name))          :interactive)
      )
  )
)

;;;###autoload
(defun +jg-python-distribute-commands (&optional dir)
  (interactive)
  (append
   (-when-let (is-pyproject (string-equal (buffer-name) "pyproject.toml"))
     (+jg-projects-pair-cmds
      '("build" "python -m build --outdir ./.temp/dist ./")
      '("upload" "gpg -q -d ~/.config/secrets/pypi/token.asc | sed -n -E 's/^pypi\\s+=\\s+(.+)/\\1/p' | xargs -I {} twine upload -u __token__ -p {} --skip-existing --non-interactive ./.temp/dist/*")
      ))
   (-when-let (is-bumpable (or (string-equal (buffer-name) "pyproject.toml")
                               (string-equal (buffer-name) "cargo.toml")
                               (string-equal (buffer-name) "bumpver.toml")))
     (+jg-projects-pair-cmds
      '("bump patch" "bumpver update --patch")
      '("bump minor" "bumpver update --minor")
      '("bump major" "bumpver update --major")
      )
     )
   )
  )
