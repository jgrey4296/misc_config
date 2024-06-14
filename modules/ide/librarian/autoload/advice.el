;;; advice.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload
(defun +lookup--projectile-find-tag-a (fn)
  (let ((xref-backend-functions '(etags--xref-backend t)))
    (funcall fn))
  )

;;;###autoload
(defun +lookup--fix-ivy-xrefs (fn fetcher alist)
  ;; HACK Fix #4386: `ivy-xref-show-xrefs' calls `fetcher' twice, which has
  ;; side effects that breaks in some cases (i.e. on `dired-do-find-regexp').
  :around #'ivy-xref-show-xrefs
  (when (functionp fetcher)
    (setf (alist-get 'fetched-xrefs alist)
          (funcall fetcher)))
  (funcall fn fetcher alist)
  )

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    May 14, 2024
;; Modified:   May 14, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; advice.el ends here
