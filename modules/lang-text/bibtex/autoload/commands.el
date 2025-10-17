;;; commands.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(defun jg-bibtex--entry-has-tags-p ()
  (not (string-empty-p (bibtex-autokey-get-field "tags")))
  )

;;;###autoload
(defun +jg-bibtex-first-entry-with-no-tags ()
  (interactive)
  (goto-char (point-min))
  (bibtex-next-entry)
  (while (and (jg-bibtex--entry-has-tags-p) (< (point) (point-max)))
    (bibtex-next-entry)
    (forward-line)
    )
  )




;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    October 17, 2025
;; Modified:   October 17, 2025
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 30.2))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;; Local Variables:
;; read-symbol-shorthands: (
;; ("blah-" . "blah-")
;; )
;; End:
;;; commands.el ends here
