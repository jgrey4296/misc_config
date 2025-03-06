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
  (when (and (f-ext? (buffer-name) "py")
             (s-matches? "test_" (f-base (buffer-name))))
    (py-test-minor-mode 1)
    (yas-activate-extra-mode 'py-test-minor-mode)
    )
  )

(defun py-test-copy-current-test ()
  (interactive)
  (while (and (not (bobp)) (not (looking-at-p "^\s+def test")))
    (python-nav-beginning-of-defun)
    )
  (let ((start (point))
        (end (progn (python-nav-end-of-defun) (point)))
        )
    (copy-region-as-kill start end)
    (insert "\n\n")

    (save-excursion
      (yank))
    (python-nav-forward-defun)
    (let ((end-point (point))
          (start-point (progn (evil-backward-WORD-begin) (point)))
          )
      (kill-region start-point end-point)
      (insert "test_")
      (evil-insert-state)
      )
    )
  )

(defun py-test-minor-function-dwim ()
  "Auto save the file then test the function point is in"
  (interactive)
  (basic-save-buffer)
  (call-interactively #'python-pytest-run-def-or-class-at-point-dwim)
  )

;;; test_mode.el ends here
