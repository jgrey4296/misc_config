;;; transient-format.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header
(require 'macro-tools--transient)

(transient-toggle-hook! ws-butler ()
  "WS-Butler"
  :key "b"
  :global t
  :fn #'ws-butler-mode
  :hook prog-mode-hook
  )

(transient-toggle-var! require-newline ()
  "Toggle final newline requirement "
  :var require-final-newline
  :key "e"
  )

;;;###autoload
(defun +jg-ui-build-transient-format ()
  (interactive)
  (transient-subgroup! jg-toggle-format-transient ()
    "For toggling text format controls"
    :key "F"
    :desc "|| Format    ||"
    [
     (transient-macro-toggle-hook-ws-butler)
     (transient-macro-toggle-require-newline)
     ]
    )

  (transient-guarded-insert-subgroup! 'jg-toggle-main jg-toggle-format-transient (1 -1))

  )

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    January 13, 2025
;; Modified:   January 13, 2025
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
;;; transient-format.el ends here
