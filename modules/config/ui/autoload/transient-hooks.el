;;; transient-hooks.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;;
;;-- end Header
(require 'macro-tools--transient)

;;;###autoload
(defun +jg-ui-build-hooks-transient ()
  (interactive)
  (transient-subgroup! jg-toggle-hooks-transient ()
    "For Controlling hooks"
    :key "k"
    :desc "|| Hooks      ||"
    []
  )

  (transient-guarded-insert-subgroup! 'jg-toggle-main jg-toggle-hooks-transient (1 -1))

  (jg-ui-run-hooks)

  )

(defun jg-ui-run-hooks ()
  (interactive)
  (cl-loop for x in macro-tools--transient-hooks
           do
           (condition-case err
               (progn (transient-guarded-append! jg-toggle-hooks-transient x (0 -1))
                      (message "----"))
             (error (message "Error: %s : %s" err x))
             )
           )
  )

;;-- Footer
;; Copyright (C) 2025 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    January 14, 2025
;; Modified:   January 14, 2025
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
;;; transient-hooks.el ends here
