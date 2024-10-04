;;; advice.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload
(defun +dired--no-revert-in-virtual-buffers-a (&rest args)
  "Don't auto-revert in dired-virtual buffers (see `dired-virtual-revert')."
  (not (eq revert-buffer-function #'dired-virtual-revert))
  )

;;;###autoload
(defun +jg-dired-find-file-with-insert-plus-a (fn &rest args)
  " A Custom read file name function to start in insert-plus state"
  (minibuffer-with-setup-hook (:append #'evil-mapspace-state)
    (apply fn args)
    )
  )


;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    May 27, 2024
;; Modified:   May 27, 2024
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
