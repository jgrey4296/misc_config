;;; envs.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

;;;###autoload
(defun jg-py-venv-start (state &rest rest)
  (let ((root (librarian--envs-loc-root (librarian--envs-state-loc state))))
    (pyvenv-activate (f-join root (or (car-safe rest) ".venv")))
    (jg-py--enter-env-update-paths state)
    )
  )

;;;###autoload
(defun jg-py-venv-stop (state &rest rest)
  (let ((root (librarian--envs-loc-root (librarian--envs-state-loc state))))
    (jg-py--exit-env-update-paths state)
    (pyvenv-deactivate)))

;;;###autoload
(defun jg-py-mamba-start-env (state &rest rest)
  (jg-py--enter-env-update-paths state)
  (micromamba-activate (car rest))
  (setenv "CONDA_DEFAULT_ENV" (car rest))
  )

;;;###autoload
(defun jg-py-mamba-stop-env (state &rest rest)
  (jg-py--exit-env-update-paths state)
  (micromamba-deactivate)
  (setenv "CONDA_DEFAULT_ENV" nil)
  )

;;;###autoload
(defun jg-py-poetry-start-env (state &rest rest)
  (jg-py--enter-env-update-paths state)
  (poetry-venv-workon))

;;;###autoload
(defun jg-py-poetry-stop-env (state &rest rest)
    (jg-py--exit-env-update-paths state)
  (poetry-venv-deactivate))

;;;###autoload
(defun jg-py--enter-env-update-paths (state)
  (let ((root (librarian--envs-loc-root (librarian--envs-state-loc state))))
    (when (boundp 'python-shell-extra-pythonpaths)
      (add-to-list python-shell-extra-pythonpaths root))
    (when (boundp 'py-shell-extra-pythonpaths)
      (add-to-list py-shell-extra-pythonpaths root))
    )
  )

;;;###autoload
(defun jg-py--exit-env-update-paths (state)
  (let ((root (librarian--envs-loc-root (librarian--envs-state-loc state))))
    (when (boundp 'python-shell-extra-pythonpaths)
      (setq python-shell-extra-pythonpaths
            (remove root python-shell-extra-pythonpaths)))
    (when (boundp 'py-shell-extra-pythonpaths)
      (setq py-shell-extra-pythonpaths
            (remove root py-shell-extra-pythonpaths)))
    )
  )

;;;###autoload
(defun jg-python-update-mypy-path (state &rest rest)
  (let* ((root (librarian--envs-loc-root (librarian--envs-state-loc state)))
         (update (list root))
         )
    (when-let* ((pyhome (getenv "PYTHONHOME"))
                (nonnull (not (string-empty-p pyhome))))
      (push pyhome update))
    (when-let* ((venv   (getenv "VIRTUAL_ENV"))
                (nonnull (not (string-empty-p venv))))
      (push venv update))

    (setenv "MYPYPATH"
            (string-join (reverse (-concat
                                   update
                                   (+jg-python-get-editable-locs)
                                   ))
                         ":"
                         )
            )
    )
  )

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    March 23, 2025
;; Modified:   March 23, 2025
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.3))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("blah-" . "blah-")
;; )
;; End:
;;; envs.el ends here
