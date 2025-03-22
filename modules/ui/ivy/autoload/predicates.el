;;; predicates.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

;;;###autoload
(defun jg-ivy-buffer-predicate-p (x)
  ;; return nil for cruft buffers
  (not (string-match jg-ibuffer-ivy-predicate-patterns (car x)))
  )

;;;###autoload
(defun +ivy--is-workspace-buffer-p (buffer)
  (let ((buffer (car buffer)))
    (when (stringp buffer)
      (setq buffer (get-buffer buffer)))
    (+workspace-contains-buffer-p buffer)))

;;;###autoload
(defun +ivy--is-workspace-other-buffer-p (buffer)
  (let ((buffer (car buffer)))
    (when (stringp buffer)
      (setq buffer (get-buffer buffer)))
    (and (not (eq buffer (current-buffer)))
         (+workspace-contains-buffer-p buffer))))

;;;###autoload
(defun +jg-ivy-file-predicate-p (x)
  (and (s-matches? jg-ivy-file-regexp x)
       (not (s-matches? jg-ivy-file-reject-regexp x)))
  )

;;;###autoload
(defun ivy-rich-bookmark-filename-or-empty-p (candidate)
(let ((filename (ivy-rich-bookmark-filename candidate)))
    (if (not filename) "" filename)))

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
;;; predicates.el ends here
