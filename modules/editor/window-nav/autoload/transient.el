;;; transient.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;-- setup
(progn
  (transient-make-var-toggle! auto-balance evil-auto-balance-windows "Auto-Balance Windows" "B")

  (transient-make-call! shrink-horizontally "h" "Horizontal Shrink" (window-resize transient--original-window -5 t t))
  (transient-make-call! shrink-vertically   "v" "Vertical Shrink"   (window-resize transient--original-window -5 nil t))
  (transient-make-call! grow-horizontally   "H" "Horizontal Grow"   (window-resize transient--original-window 5 t t))
  (transient-make-call! grow-vertically     "V" "Vertical Grow"     (window-resize transient--original-window 5 nil t))

  (transient-make-call! toggle-layout       "/" "Toggle Layout"     (+jg-ui-window-layout-toggle))
  (transient-make-call! rotate-layout       "\\" "Rotate Layout"    (+jg-ui-window-rotate-forward))


  (transient-make-call! toggle-dedication   "!"
                        (format "  %s : Window Dedicated"
                                (fmt-as-bool! (window-dedicated-p (selected-window))))
                        (let ((curr-window (selected-window)))
                          (set-window-dedicated-p curr-window (not (window-dedicated-p curr-window)))
                          (if (window-dedicated-p curr-window)
                              (message "Window is now dedicated to %s" (window-buffer curr-window))
                            (message "Window is un-dedicated"))
                          )
                        )

  (transient-make-int-call! window-delete      "d" "Delete Window"   #'+workspace/close-window-or-workspace)

  (transient-make-int-call! window-split-below "-" "Split Below"     #'split-window-below)
  (transient-make-int-call! window-split-right "=" "Split Right"     #'split-window-right)
  (transient-make-int-call! window-maximize    "m" "Maximize Window" #'doom/window-maximize-buffer)
  ;; TODO move these to window-nav
  (transient-make-int-call! window-undo        "u" "Window Undo"     #'winner-undo)
  (transient-make-int-call! window-redo        "U" "Window Redo"     #'winner-redo)
  (transient-make-call!     window-balance     "b" "Balance Windows" (balance-windows))
  )

;;-- end setup


;;;###autoload
(defun +jg-windows-add-transients ()
  " Add window control to the workspace transient "
  (transient-append-suffix 'workspace-control-transient "RET" '(transient-macro-call-toggle-dedication))
  (transient-append-suffix 'workspace-control-transient "RET" '(transient-macro-toggle-auto-balance))

  (transient-append-suffix 'workspace-control-transient '(-2)
    [ ;; row
     ["Window Sizes" ;; col
      (transient-macro-call-shrink-horizontally)
      (transient-macro-call-shrink-vertically)
      (transient-macro-call-grow-horizontally)
      (transient-macro-call-grow-vertically)
      (transient-macro-call-window-maximize)
      (transient-macro-call-window-balance)
      ]
     ["Window Layouts" ;; col
      (transient-macro-call-toggle-layout)
      (transient-macro-call-rotate-layout)
      (transient-macro-call-window-split-below)
      (transient-macro-call-window-split-right)
      (transient-macro-call-window-delete)
      ]
     ["Winner" ;; col
      (transient-macro-call-window-undo)
      (transient-macro-call-window-redo)
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
;;; transient.el ends here
