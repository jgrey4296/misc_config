;;; zimmerframe-transient.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header
(require 'transient)

(defun +jg-workspace-zimmerframe-active-title ()
  (format "Zimmer (%s) Control" (fmt-as-bool! project-zimmerframe-minor-mode))
  )

(defun +jg-workspace-zimmerframe-group-title ()
  (format "+Zimmerframe : %s" (if (fboundp 'zimmerframe-remaining-count)
                                  (zimmerframe-remaining-count)
                                0))
  )
(progn
  (transient-make-mode-toggle! project-zimmerframe-minor-mode "Zimmerframe" "RET")
  (transient-make-int-call! zimmerframe-default-filters "SPC" "Apply Default Filters"        #'zimmerframe-filter-defaults)
  (transient-make-int-call! zimmerframe-remaining "b" "Remaining List Buffer" :transient nil #'zimmerframe-remaining)
  (transient-make-int-call! zimmerframe-count   "r"  "Remaining Count"                       #'zimmerframe-num)
  (transient-make-int-call! zimmerframe-dir     "d"  "Set Target Directory"                  #'zimmerframe-directory-init)
  (transient-make-int-call! zimmerframe-filter  "f"  "Filter "                               #'zimmerframe-filter)
  (transient-make-int-call! zimmerframe-keep    "k"  "Keep"                                  #'zimmerframe-filter-keep)
  (transient-make-int-call! zimmerframe-replace "R"  ": Replace Regexp"                        #'zimmerframe-replace-regexp)
  (transient-make-call! zimmerframe-next         "w" ": Walk Next"                 (zimmerframe-next))
  (transient-make-call! zimmerframe-prev         "W" ": Walk Prev"                 (zimmerframe-prev))
  )

(transient-make-subgroup! transient-zimmerframe "z"
                          ""
                          [:description +jg-workspace-zimmerframe-group-title
                           ["Mode"
                            (transient-macro-toggle-project-zimmerframe-minor-mode)
                            ]
                           ]
                          [
                           ["Inspect"
                            (transient-macro-call-zimmerframe-remaining)
                            (transient-macro-call-zimmerframe-count)
                            ]
                           ["Filter"
                            (transient-macro-call-zimmerframe-default-filters)
                            (transient-macro-call-zimmerframe-filter)
                            (transient-macro-call-zimmerframe-keep)
                            ]
                           ["Transform"
                            (transient-macro-call-zimmerframe-replace)
                            ]
                           ]
                          )

;;;###autoload
(defun +jg-workspace-add-zimmerframe-transient ()
  (transient-append-suffix 'workspace-control-transient '(0 -1 -1) '(transient-macro-toggle-project-zimmerframe-minor-mode))
  (transient-remove-suffix 'workspace-control-transient "1")

  (transient-append-suffix 'workspace-control-transient
    '(1 0 0)
     transient-zimmerframe
    )
  (transient-append-suffix 'workspace-control-transient
    '(1)
     [:description +jg-workspace-zimmerframe-active-title
      [(transient-macro-call-zimmerframe-next)]
      [(transient-macro-call-zimmerframe-prev)]
      [(transient-macro-call-zimmerframe-replace)]
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
;;; zimmerframe-transient.el ends here
