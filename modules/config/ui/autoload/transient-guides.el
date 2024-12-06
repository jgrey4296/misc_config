;;; transient-guides.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;; Guides
(progn
  (transient-make-mode-toggle! global-prettify-symbols-mode  "Pretty Symbols" "p")

  (transient-make-mode-toggle! display-fill-column-indicator-mode  (format "Columns : %-3s" fill-column) "c")
  (transient-make-mode-toggle! highlight-indent-guides-mode        "Indents"      "i")
  (transient-make-mode-toggle! ruler-mode                          "Ruler"        "r")
  (transient-make-mode-toggle! whitespace-mode                     "Whitespace"   "w")
  (transient-make-mode-toggle! display-line-numbers-mode           "Line Numbers" "n")
  (transient-make-mode-toggle! glasses-mode                        "Glasses" "g")

  ;;
  (transient-make-call!   spelling "s"
                          (transient-title-mode-formatter "Spelling" flyspell-mode "s")
                          (flyspell-mode 'toggle)
                          (writegood-mode (if flyspell-mode 1 -1))
                          )

  )

;;;###autoload
(defun +jg-ui-build-guides-transient ()
  (transient-make-subgroup! jg-toggle-guides-transient "g"
                          "For controlling ui guide settings"
                          :desc "|| Guides     ||"
                          [:description "|| Guides     ||"
                           [
                            (transient-macro-toggle-display-fill-column-indicator-mode)
                            (transient-macro-toggle-highlight-indent-guides-mode)
                            (transient-macro-toggle-display-line-numbers-mode)
                            ]
                           [
                            (transient-macro-toggle-ruler-mode)
                            (transient-macro-toggle-whitespace-mode)
                            (transient-macro-call-spelling)
                            ]
                           [
                            (transient-macro-toggle-glasses-mode)
                            ]
                           ]
                          )

  (pcase (transient-get-suffix 'jg-toggle-main '(1 -1))
         ((and `[1 transient-columns nil ,x]
               (guard (< (length x) 4)))
          (transient-append-suffix 'jg-toggle-main
              '(1 -1 -1) jg-toggle-guides-transient))
         (t (transient-append-suffix 'jg-toggle-main
            '(1 -1) [ jg-toggle-guides-transient ]))
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
;;; transient-guides.el ends here
