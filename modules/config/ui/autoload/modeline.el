;;; modeline.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(defvar doom-modeline--formatters nil)
(defvar doom-modeline--current nil)

;;;###autoload
(defun doom-modeline--get-formatters ()
  (cl-do-all-symbols (sym)
    (when (s-starts-with? "doom-modeline-format--" (symbol-name sym))
      (add-to-list 'doom-modeline--formatters (s-chop-prefixes '("doom-modeline-format--") (symbol-name sym)))
      )
    )
  (setq doom-modeline--formatters (sort doom-modeline--#'string-lessp))
  )

;;;###autoload
(defun +jg-ui-modeline-choose (arg)
  " Set the current modeline format "
  (interactive "P")
  (when (or arg (not doom-modeline--formatters))
    (doom-modeline--get-formatters))
  (ivy-read (format "(%s) Modeline: " (or doom-modeline--current ""))
            doom-modeline--formatters
            :action #'(lambda (x) (doom-modeline-set-modeline (intern-soft x)))
            )
  )

;;;###autoload
(defun +jg-ui-modeline-segments (arg)
  " Insert the name of a modeline segment "
  (interactive "P")
  (ivy-read "Modeline Segments: "
            (mapcar #'car doom-modeline-fn-alist)
            :action #'insert
            )
  )

;;;###autoload
(defun +jg-ui-modeline-record-ad (key &rest args)
  (setq doom-modeline--current key)
  )


;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    March 06, 2025
;; Modified:   March 06, 2025
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
;;; modeline.el ends here
