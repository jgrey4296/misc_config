;;; hooks.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

;;;###autoload
(defun +spell-init-excluded-faces-h ()
  "Set `spell-fu-faces-exclude' according to `+spell-excluded-faces-alist'."
  (when-let (excluded (cdr (cl-find-if #'derived-mode-p +spell-excluded-faces-alist :key #'car)))
    (setq-local spell-fu-faces-exclude excluded))
  )

;;;###autoload
(defun +spell-inhibit-duplicate-detection-maybe-h ()
  "Don't mark duplicates when style/grammar linters are present.
e.g. proselint and langtool."
  (and (or (and (bound-and-true-p flycheck-mode)
                (executable-find "proselint"))
           (featurep 'langtool))
       (setq-local flyspell-mark-duplications-flag nil))
  )

;;;###autoload
(defun +spell-remove-run-together-switch-for-aspell-h ()
  (setq-local ispell-extra-args (remove "--run-together" ispell-extra-args))
  )

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    March 21, 2025
;; Modified:   March 21, 2025
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
;;; hooks.el ends here
