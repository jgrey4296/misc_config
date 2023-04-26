;;; +env.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: July 10, 2022
;; Modified: July 10, 2022
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.1))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Code for activating environments,
;;  both pvenv and conda
;;
;;; Code:


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

;;; +env.el ends here
