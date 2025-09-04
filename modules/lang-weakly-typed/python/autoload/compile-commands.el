;;; compile-commands.el -*- lexical-binding: t; no-byte-compile: t-*-

;;;###autoload
(defun +jg-python-get-commands (&optional dir)
  (interactive)
  (-when-let* ((root (projectile-project-root dir))
               (proj-name (projectile-project-name))
               (proj-src (-if-let* ((loc (f-join root proj-name))
                                    (exists (f-exists? loc))
                                    )
                             loc
                           root))
               (project (f-join root "pyproject.toml"))
               (project-exists (f-exists? project))
               (curr-file (buffer-file-name))
               )
    (-reject #'null (+jg-eval--pair-cmds
     ;; testing
     `("debug" ,(format "echo 'Test: %s'" curr-file))
     ;; uv
     '("uv sync" "uv sync --all-extras --all-groups")
     '("uv deps" "uv tree --all-groups")
     ;; pytest
     '("pytest" "pytest")
     '("pytest version" "pytest --version")
     ;; Mypy
     `("mypy file"    ,(format "mypy %s" curr-file) :interactive)
     `("mypy nocache" ,(format "mypy --no-incremental %s" curr-file) :interactive)
     `("mypy tb"      ,(format "mypy --show-traceback %s" curr-file) :interactive)
     ;; Tox
     `("tox" "tox")
     ;; Sphinx
     `("sphinx fresh" ,(format "uv run sphinx-build --fresh-env --conf-dir %s --warning-file %s --builder html %s %s"
                              (f-join proj-src "_docs")
                              (f-join root ".temp/logs/sphinx.log")
                              proj-src
                              (f-join root ".temp/docs")))
     `("sphinx docs" ,(format "uv run sphinx-build --conf-dir %s --warning-file %s --builder html %s %s"
                              (f-join proj-src "_docs")
                              (f-join root ".temp/logs/sphinx.log")
                              proj-src
                              (f-join root ".temp/docs")))
     `("sphinx quiet" ,(format "uv run sphinx-build -q --conf-dir %s --warning-file %s --builder html %s %s"
                               (f-join proj-src "_docs")
                               (f-join root ".temp/logs/sphinx.log")
                               proj-src
                               (f-join root ".temp/docs")))

     ;; TownCrier changes
     `("change" "towncrier create --no-edit" :read)
     `("draft changelog" "towncrier build --draft")
     `("build changelog" "towncrier build --keep")

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
     `("run-py"          ,(format "python -X dev %s" filename)       :interactive)
     `("run-py-verbose"  ,(format "python -X dev -i -v %s" filename) :interactive)
     `("run-interactive" ,(format "ipython -i %s" filename)          :interactive)
      )
  )
)
