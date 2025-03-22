;;; wrappers.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

;;;###autoload
(defun +ivy/compile ()
  "Execute a compile command from the current buffer's directory."
  (interactive)
  ;; Fix unhelpful 'Couldn't find project root' error
  (letf! (defun counsel--compile-root ()
           (ignore-errors
             (funcall counsel--compile-root)))
    (counsel-compile default-directory)))

;;;###autoload
(defun +ivy/project-compile ()
  "Execute a compile command from the current project's root."
  (interactive)
  (counsel-compile (projectile-project-root)))

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    March 22, 2025
;; Modified:   March 22, 2025
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
;;; wrappers.el ends here
