;;; outline.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

;;;###autoload
(defun outline-set-buffer-local-ellipsis (&optional ellipsis)
  "Apply the ellipsis ELLIPSIS to outline mode locally to a buffer.

from https://www.jamescherti.com/emacs-customize-ellipsis-outline-minor-mode/ "
  (let* ((display-table (or buffer-display-table (make-display-table)))
         (face-offset (* (face-id 'shadow) (ash 1 22)))
         (value (vconcat (mapcar (lambda (c) (+ face-offset c)) (or ellipsis jg-outline-ellipsis))))
         )
    (set-display-table-slot display-table 'selective-display value)
    (setq buffer-display-table display-table))
  )


;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    May 06, 2025
;; Modified:   May 06, 2025
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
;;; outline.el ends here
