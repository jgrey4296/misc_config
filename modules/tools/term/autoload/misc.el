;;; misc.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header

;;;###autoload
(defun +sh-init-extra-fontification-h ()
  (font-lock-add-keywords
   nil `((+sh--match-variables-in-quotes
          (1 'font-lock-constant-face prepend)
          (2 'font-lock-variable-name-face prepend))
         (+sh--match-command-subst-in-quotes
          (1 'sh-quoted-exec prepend))
         (,(regexp-opt +sh-builtin-keywords 'symbols)
          (0 'font-lock-type-face append)))
    )
  )

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    September 29, 2025
;; Modified:   September 29, 2025
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
;;; misc.el ends here
