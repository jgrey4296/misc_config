;;; transient-wrap.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;; Wrap
(transient-toggle-mode! visual-line-mode ()
  "Visual line"
  :key "l"
  )
(transient-toggle-mode! +word-wrap-mode ()
  "Word-wrap"
  :key "w"
  )
(transient-toggle-mode! auto-fill-mode ()
  "Auto-fill"
  :key "f"
  :mode-var auto-fill-function
  )

(transient-call! truncate-lines ()
  "Truncate Lines"
  :key "t"
  :desc (format "%3s : Truncate lines" (fmt-as-bool! truncate-lines))
  (toggle-truncate-lines)
  )

;;;###autoload
(defun +jg-ui-build-wrap-transient ()
  (transient-subgroup! jg-toggle-wrap-transient ()
    "For controlling ui wrap settings"
    :key "w"
    :desc "|| Wrapping   ||"
    [[:description "|| Wrapping ||"
      (transient-macro-toggle-auto-fill-mode)
      (transient-macro-toggle-visual-line-mode)
      (transient-macro-toggle-+word-wrap-mode)
      (transient-macro-call-truncate-lines)
      ]
     ]
    )

  (pcase (transient-get-suffix 'jg-toggle-main '(1 -1))
    ((and `[1 transient-columns nil ,x]
          (guard (< (length x) 4)))
     (transient-append-suffix 'jg-toggle-main
       '(1 -1 -1)  jg-toggle-wrap-transient))
    (_ (transient-append-suffix 'jg-toggle-main
         '(1 -1)  [ jg-toggle-wrap-transient ]))
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
