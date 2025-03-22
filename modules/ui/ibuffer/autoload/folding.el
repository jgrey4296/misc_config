;;; folding.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

;;;###autoload
(defun +jg-ibuffer-fold-all-groups ()
  (interactive)
  (save-excursion
    (setq ibuffer-hidden-filter-groups
    (cl-loop for group in (mapcar #'car ibuffer-filter-groups)
             with curr-groups = (ibuffer-current-filter-groups-with-position)
             when (assoc group curr-groups)
             collect group)))
  (ibuffer-redisplay t)
  )

(defun +jg-ibuffer-unfold-all-groups ()
  (interactive)
  (setq ibuffer-hidden-filter-groups nil)
  (ibuffer-update nil)
  )

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
;;; folding.el ends here
