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

;;; test_mode.el ends here
