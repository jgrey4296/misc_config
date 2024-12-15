;;; transient-debug.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;; Debug
(progn
  (transient-make-call! debug-on-error "e"
                        (transient-title-mode-formatter "Debug on Error" debug-on-error "e")
                        (toggle-debug-on-error))
  (transient-make-call! debug-on-var "v"
                        (transient-title-mode-formatter "Debug on Var" (debug--variable-list) "v")
                        (call-interactively #'debug-on-variable-change))
  (transient-make-call! cancel-debug-on-var "V"
                        "Cancel All Var Debugs"
                        (cancel-debug-on-variable-change))
  (transient-make-call! debug-func "f"
                        (transient-title-mode-formatter "Debug on Fn" (debug--function-list) "f")
                        (call-interactively #'debug-on-entry))
  (transient-make-call! cancel-debug-func "F"
                        "Cancel All Function Debugs"
                        (cancel-debug-on-entry))
  )

;;;###autoload
(defun +jg-ui-build-debugs-transient ()
  (transient-make-subgroup! jg-toggle-debugs-transient "d"
                            " debug toggles "
                            :desc  "|| Debug      ||"
                            [:description "|| Debug      ||"
                                          [
                                           (transient-macro-call-debug-on-error)
                                           (transient-macro-call-debug-on-var)
                                           (transient-macro-call-debug-func)
                                           ]
                                          [
                                           " "
                                           (transient-macro-call-cancel-debug-on-var)
                                           (transient-macro-call-cancel-debug-func)

                                           ] ]
                            )

  (transient-append-suffix 'jg-toggle-main
    '(2 0 -1)
    '(transient-macro-call-debug-on-error)
    )

  (pcase (transient-get-suffix 'jg-toggle-main '(1 -1))
    ((and `[1 transient-columns nil ,x]
          (guard (< (length x) 4)))
     (transient-append-suffix 'jg-toggle-main
       '(1 -1 -1)  jg-toggle-debugs-transient))
    (_ (transient-append-suffix 'jg-toggle-main
       '(1 -1)  [ jg-toggle-debugs-transient ]))
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
;;; transient-debug.el ends here
