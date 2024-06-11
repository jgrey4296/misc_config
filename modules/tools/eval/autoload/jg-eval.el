;;; jg-eval.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;; adapated from doom's eval
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload
(defun +jg-eval-run-region-or-str (beg end &optional str)
  (interactive)

  )

;;;###autoload
(defun +jg-eval-run-buffer ()
  (interactive)
  (+jg-eval-run-region-or-str (point-min) (point-max))
  )

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    April 03, 2024
;; Modified:   April 03, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; jg-eval.el ends here
