;;; flycheck.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;; An adaptation of flychecks ruff checker
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload
(defun jg-define-override-ruff-checker ()
  (require #'flycheck)

  (flycheck-def-config-file-var flycheck-python-ruff-config python-ruff
                                '("pyproject.toml" "ruff.toml" ".ruff.toml"))

  (flycheck-define-checker jg-python-ruff
    "A Python syntax and style checker using the ruff.
To override the path to the ruff executable, set
`flycheck-python-ruff-executable'.

See URL `https://docs.astral.sh/ruff/"
    :command ("ruff"
              "check"
              (config-file "--config" flycheck-python-ruff-config)
              "--output-format=concise"
              (option "--stdin-filename" buffer-file-name)
              "-")
    :standard-input t
    :error-filter (lambda (errors)
                    (let* ((errors (flycheck-sanitize-errors errors))
                           (errors-with-ids (seq-filter #'flycheck-error-id errors)))
                      (seq-union
                       (seq-difference errors errors-with-ids)
                       (seq-map #'flycheck-flake8-fix-error-level errors-with-ids))))
    :error-patterns
    ((error line-start
            (or "-" (file-name)) ":" line ":" (optional column ":") " "
            "invalid-syntax: "
            (message (one-or-more not-newline))
            line-end)
     (warning line-start
              (or "-" (file-name)) ":" line ":" (optional column ":") " "
              (id (one-or-more (any alpha)) (one-or-more digit) " ")
              (message (one-or-more not-newline))
              line-end))
    :modes (python-mode python-ts-mode)
    :next-checkers ((warning . python-mypy))
    )
  (add-to-list 'flycheck-checkers 'jg-python-ruff)
  )

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    May 05, 2024
;; Modified:   May 05, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; flycheck.el ends here
