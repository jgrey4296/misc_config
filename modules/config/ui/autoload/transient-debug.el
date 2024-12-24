;;; transient-debug.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header
(require 'macro-tools--transient)

;; Debug
(transient-call! debug-on-error ()
  ""
  :key "e"
  :desc (transient-var-fmt "Debug on Error" debug-on-error "e")
  (toggle-debug-on-error)
  )
(transient-call! debug-on-var ()
  ""
  :key "v"
  :desc (transient-var-fmt "Debug on Var" (debug--variable-list) "v")
  :interactive t
  #'debug-on-variable-change
  )
(transient-call! cancel-debug-on-var ()
  ""
  :key "V"
  :desc "Cancel All Var Debugs"
  (cancel-debug-on-variable-change)
  )
(transient-call! debug-func ()
  ""
  :key "f"
  :desc (transient-var-fmt "Debug on Fn" (debug--function-list) "f")
  :interactive t
  #'debug-on-entry
  )
(transient-call! cancel-debug-func ()
  ""
  :key "F"
  :desc "Cancel All Function Debugs"
  (cancel-debug-on-entry)
  )

;;;###autoload
(defun +jg-ui-build-debugs-transient ()
  (transient-subgroup! jg-toggle-debugs-transient ()
    "debug toggles "
    :key "d"
    :desc  "|| Debug      ||"
    [
     (transient-macro-call-debug-on-error)
     (transient-macro-call-debug-on-var)
     (transient-macro-call-debug-func)
     ]
    [
     " "
     (transient-macro-call-cancel-debug-on-var)
     (transient-macro-call-cancel-debug-func)
     ]
    )

  (transient-append-suffix 'jg-toggle-main
    '(2 0 -1)
    '(transient-macro-call-debug-on-error)
    )

  (transient-guarded-insert! 'jg-toggle-main jg-toggle-debugs-transient (1 -1))
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
;;; transient-debug.el ends here
