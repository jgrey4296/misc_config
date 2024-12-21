;;; transient.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;;-- setup
(transient-toggle-var! auto-balance ()
  "Auto-Balance Windows"
  :var evil-auto-balance-windows
  :key "B"
  )

(transient-call! shrink-horizontally ()
  "Horizontal Shrink"
  :key "h"
  (window-resize transient--original-window -5 t t)
  )
(transient-call! shrink-vertically   ()
  "Vertical Shrink"
  :key "v"
  (window-resize transient--original-window -5 nil t)
  )
(transient-call! grow-horizontally   ()
  "Horizontal Grow"
  :key "H"
  (window-resize transient--original-window 5 t t)
  )
(transient-call! grow-vertically ()
  "Vertical Grow"
  :key "V"
  (window-resize transient--original-window 5 nil t)
  )

(transient-call! toggle-layout ()
  "Toggle Layout"
  :key "/"
  (+jg-ui-window-layout-toggle)
  )
(transient-call! rotate-layout ()
  "Rotate Layout"
  :key "\\"
  (+jg-ui-window-rotate-forward)
  )

(transient-call! toggle-dedication ()
  "Window Dedication"
  :key "!"
  :desc (format "  %s : Window Dedicated"
                (fmt-as-bool! (window-dedicated-p (selected-window))))
  (let ((curr-window (selected-window)))
    (set-window-dedicated-p curr-window (not (window-dedicated-p curr-window)))
    (if (window-dedicated-p curr-window)
        (message "Window is now dedicated to %s" (window-buffer curr-window))
      (message "Window is un-dedicated"))
    )
  )
(transient-call! window-balance ()
  "Balance Windows"
  :key "b"
  (balance-windows)
  )
(transient-call! window-delete      ()
  "Delete Window"
  :key "d"
  :interactive t
  #'+workspace/close-window-or-workspace
  )
(transient-call! window-split-below ()
  "Split Below"
  :key "-"
  :interactive t
  #'split-window-below
  )
(transient-call! window-split-right ()
  "Split Right"
  :key "="
  :interactive t
  #'split-window-right
  )
(transient-call! window-maximize    ()
  "Maximize Window"
  :key "m"
  :interactive t
  #'doom/window-maximize-buffer
  )

;; TODO move these to window-nav
(transient-call! window-undo        ()
  "Window Undo"
  :key "u"
  :interactive t
  #'winner-undo
  )
(transient-call! window-redo        ()
  "Window Redo"
  :key "U"
  :interactive t
  #'winner-redo
  )

;;-- end setup

;;;###autoload
(defun +jg-windows-add-transients ()
  " Add window control to the workspace transient "
  (transient-append-suffix 'workspace-control-transient '(0 -1 -1) '(transient-macro-call-toggle-dedication))
  (transient-append-suffix 'workspace-control-transient '(0 -1 -1) '(transient-macro-toggle-auto-balance))
  (transient-remove-suffix 'workspace-control-transient "1")

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
