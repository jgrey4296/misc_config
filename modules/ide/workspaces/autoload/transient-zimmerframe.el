;;; zimmerframe-transient.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header
(require 'macro-tools--transient)

(defun +jg-workspace-zimmerframe-active-title ()
  (format "Zimmer (%s) Control" (fmt-as-bool! project-zimmerframe-minor-mode))
  )
(defun +jg-workspace-zimmerframe-group-title ()
  (format "+Zimmerframe : %s" (if (fboundp 'zimmerframe-remaining-count)
                                  (zimmerframe-remaining-count)
                                0))
  )

(transient-toggle-mode! project-zimmerframe-minor-mode ()
  "Zimmerframe"
  :key "RET"
  )
(transient-call! zimmerframe-default-filters ()
  "Apply Default Filters"
  :key "SPC"
  :interactive t
  #'zimmerframe-filter-defaults)
(transient-call! zimmerframe-remaining ()
  "Remaining List Buffer"
  :key "b"
  :transient nil
  :interactive t
  #'zimmerframe-remaining)
(transient-call! zimmerframe-count ()
  "Remaining Count"
  :key "r"
  :interactive t
  #'zimmerframe-num)
(transient-call! zimmerframe-dir ()
  "Set Target Directory"
  :key "d"
  :interactive t
  #'zimmerframe-directory-init)
(transient-call! zimmerframe-filter ()
  "Filter "
  :key "f"
  :interactive t
  #'zimmerframe-filter)
(transient-call! zimmerframe-keep  ()
  "Keep"
  :interactive t
  :key "k"
  #'zimmerframe-filter-keep)
(transient-call! zimmerframe-replace ()
  "Replace Regexp across project"
  :key "R"
  :interactive t
  :desc (macro-tools--transient-simple-fmt "Replace Regexp" "R")
  #'zimmerframe-replace-regexp)
(transient-call! zimmerframe-next ()
  "Next"
  :key "l"
  :desc (macro-tools--transient-simple-fmt "Walk Next" "l")
  (zimmerframe-next))
(transient-call! zimmerframe-prev ()
  "Prev"
  :key "h"
  :desc (macro-tools--transient-simple-fmt "Walk Prev" "h")
  (zimmerframe-prev))

(defun jg-workspace-build--zimmerframe-transient-group ()
  (transient-subgroup! transient-zimmerframe ()
    ""
    :key "z"
    :desc +jg-workspace-zimmerframe-group-title
     [""
      (transient-macro-toggle-project-zimmerframe-minor-mode)
      ]
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
    )
  )

;;;###autoload
(defun jg-workspace-build-zimmerframe-transient ()
  (jg-workspace-build--zimmerframe-transient-group)

  (transient-append-suffix 'workspace-control-transient '(-2)
    `[:description +jg-workspace-zimmerframe-active-title
                  [
                   (transient-macro-call-zimmerframe-next)
                   (transient-macro-call-zimmerframe-prev)
                   (transient-macro-call-zimmerframe-replace)
                   ]
                  [
                  ,transient-zimmerframe
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
;;; zimmerframe-transient.el ends here
