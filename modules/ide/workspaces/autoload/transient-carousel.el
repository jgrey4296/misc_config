;;; carousel-transient.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header
(require 'transient)

(progn
  (transient-make-call! carousel-print       "p" "Print Order" (carousel-print-order))
  (transient-make-call! carousel-edit        "E" "Edit Order" :transient nil (carousel-edit-order))
  (transient-make-call! carousel-toggle-loop "l"
                        (format              "Looping : %s" (fmt-as-bool! (persp-parameter 'carousel-loop)))
                        (carousel-toggle-loop))
  (transient-make-call! carousel-new         "n" "New Carousel"   :transient nil (carousel-new))
  (transient-make-call! carousel-toggle      "t" "Toggle"         (carousel-toggle))
  (transient-make-call! carousel-quit        "T" "Quit"           (carousel-deconvert))
  (transient-make-call! carousel-expand      "e" "Expand Focus"   :transient nil (call-interactively #'carousel-expand-focus))
  (transient-make-call! carousel-reset       "r" "Reset"          :transient nil (carousel-reset-columns))
  (transient-make-call! carousel-clear       "K" "Clear Carousel" :transient nil (carousel-clear-ring))
  (transient-make-call! carousel-add-buffer  "a" "Add"            :transient nil (carousel-add-current-buffer))
  (transient-make-call! carousel-remove      "x" "Remove"         :transient nil (carousel-remove-buffer))
  (transient-make-call! carousel-move-left   "[" "Move Left" (carousel-move-buffer-left))
  (transient-make-call! carousel-move-right  "]" "Move Right" (carousel-move-buffer-right))
  (transient-make-call! carousel-claim       "w" "Claim Window" (carousel-claim-window))

  (transient-make-int-call! carousel-goto        "f" "Find-Buffer" :transient nil :interactive t #'carousel-goto-choice)
  )

(defun jg-workspace-carousel-title ()
  (format "Carousel: %s (%s)"
          (fmt-as-bool! (persp-parameter 'carousel))
          (if (persp-parameter 'carousel)
              (carousel--length (persp-parameter 'carousel-actual))
            "0")
          )

  )

(defun jg-workspace-define-carousel-transient ()
  (transient-define-prefix transient-carousel ()
    ""
    [:description jg-workspace-carousel-title
     [(transient-macro-call-carousel-toggle)
      (transient-macro-call-carousel-new)
      ]
     [(transient-macro-call-carousel-quit)
      (transient-macro-call-carousel-reset)
      ]
     [(transient-macro-call-carousel-toggle-loop)
      (transient-macro-call-carousel-clear)
      ]
     ]
    ["Order Change"
     [(transient-macro-call-carousel-move-left)]
     [(transient-macro-call-carousel-move-right)]
     ]
    [
     ["Buffer Control "
     (transient-macro-call-carousel-claim)
     (transient-macro-call-carousel-expand)
     (transient-macro-call-carousel-goto)
     ]
    [" "
     (transient-macro-call-carousel-add-buffer)
     (transient-macro-call-carousel-remove)
     (transient-macro-call-carousel-edit)
     (transient-macro-call-carousel-print)
     ]
    ]
  transient-quit!
  )
)

(defun jg-workspace-run-carousel-transient ()
  (interactive)
  (let ((transient--buffer-name "*Carousel-Transient*"))
    (transient-carousel)
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
;;; carousel-transient.el ends here
