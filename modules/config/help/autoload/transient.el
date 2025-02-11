;;; transient.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

(transient-toggle-hook! eldoc ()
  "Eldoc control"
  :hook 'prog-mode
  :fn #'eldoc-mode
  :global t
  :key "e"
  )


;;;###autoload
(defun +jg-help-build-transient ()
  "nothing yet"
  (interactive)

  )


;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    February 10, 2025
;; Modified:   February 10, 2025
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
;;; transient.el ends here
