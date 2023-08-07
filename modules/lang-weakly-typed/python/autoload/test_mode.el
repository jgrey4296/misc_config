;;; test_mode.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- header
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: June 06, 2023
;; Modified: June 06, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.2))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;;-- end header

;;;###autoload
(define-minor-mode py-test-minor-mode
  "For Simple layering on top of python test files"
  :keymap (make-sparse-keymap)
  )

;;;###autoload
(defun maybe-py-test-minor-mode ()
  (interactive)
  (if (and (f-ext? (buffer-name) "py")
           (s-matches? "test_" (f-base (buffer-name))))
      (py-test-minor-mode 1)
      )
  )

(defun py-test-copy-current-test ()
  (interactive)
  (let ((start (progn (python-nav-beginning-of-defun)
                      (point)))
        (end (progn (python-nav-end-of-defun)
                    (point)))
        )
    (copy-region-as-kill start end)
    (insert "\n\n")
    (yank)
    (python-nav-backward-defun)
    (let ((start-name (re-search-forward "def " (line-end-position)))
          (end-name (re-search-forward "(" (line-end-position))))
      (kill-region start-name end-name))
    (insert "test_(")
    (backward-char 1)
    (evil-insert-state)
    )
  )

(defun py-test-minor-function-dwim ()
  "Auto save the file then test the function point is in"
  (interactive)
  (basic-save-buffer)
  (call-interactively #'python-pytest-function-dwim)
  )

;;; test_mode.el ends here
