;;; sorting.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;;###autoload (autoload 'ibuffer-do-sort-by-marked "ui/ibuffer/autoload/sorting.el" nil t)
(define-ibuffer-sorter marked
  " Sort buffers by their marked status "
  (:description "marked")
  ;; non-nill if a < b
  (let ((a-marked (-contains? +jg-ibuffer-marked-list (car a)))
        (b-marked (-contains? +jg-ibuffer-marked-list (car b)))
        )
    (cond ((and a-marked (not b-marked)) t)
          ((and (not a-marked) b-marked) nil)
          (t
           (string-lessp (downcase (buffer-name (car a)))
                         (downcase (buffer-name (car b)))
                         )
           )
          )
    )
  )

;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    April 29, 2024
;; Modified:   April 29, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; sorting.el ends here
