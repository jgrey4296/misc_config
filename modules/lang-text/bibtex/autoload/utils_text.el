;;; utils_text.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload
(defun +jg-bibtex-title-case (x)
  (let ((case-fold-search nil))
    (string-join (cl-loop for x in (split-string x " +" t " +")
                          collect
                          (cond ((s-matches-p (rx word-start (or "and" (+ upper-case)) word-end ) x)
                                 x)
                                (t (capitalize x))
                                )
                          )
                 " ")
    )
  )


;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    February 09, 2024
;; Modified:   February 09, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; utils_text.el ends here
