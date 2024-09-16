;;; transient-wrap.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;; Wrap
(progn
  (transient-make-mode-toggle! visual-line-mode  "Visual line" "l")
  (transient-make-mode-toggle! +word-wrap-mode   "Word-wrap"   "w")
  (transient-make-mode-toggle! auto-fill-mode    "Auto-fill"   "f" nil auto-fill-function)

  (transient-make-call! truncate-lines "t"
                        (format "%-2s : Truncate lines" (fmt-as-bool! truncate-lines))
                        (toggle-truncate-lines)
                        )

  )

;;;###autoload
(defun +jg-ui-build-wrap-transient ()
  (transient-make-subgroup! jg-toggle-wrap-transient "w"
                          "For controlling ui wrap settings"
                          :desc "|| Wrapping   ||"
                          [[
                            (transient-macro-toggle-auto-fill-mode)
                            (transient-macro-toggle-visual-line-mode)
                            (transient-macro-toggle-+word-wrap-mode)
                            (transient-macro-call-truncate-lines)
                            ]
                           ]
                          )
  )




;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    August 22, 2024
;; Modified:   August 22, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; transient-wrap.el ends here
