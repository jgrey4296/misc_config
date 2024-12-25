;;; carousel-transient.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header
(require 'macro-tools--transient)

(transient-call! carousel-print ()
  ""
  :key "p"
  :desc (macro-tools--transient-simple-fmt "Print Order" "p")
  (carousel-print-order))
(transient-call! carousel-edit ()
  ""
  :key "E"
  :desc (macro-tools--transient-simple-fmt "Edit Order" "E")
  :transient nil
  (carousel-edit-order))
(transient-call! carousel-toggle-loop ()
  ""
  :key "l"
  :desc (transient-var-fmt "Looping" (persp-parameter 'carousel-loop) "l")
  (carousel-toggle-loop))
(transient-call! carousel-new ()
  ""
  :key "n"
  :desc (macro-tools--transient-simple-fmt "New Carousel" "n")
  :transient nil
  (carousel-new))
(transient-call! carousel-toggle ()
  ""
  :key "t"
  :desc (macro-tools--transient-simple-fmt "Toggle" "t")
  (carousel-toggle))
(transient-call! carousel-quit ()
  ""
  :key "T"
  :desc (macro-tools--transient-simple-fmt "Quit" "T")
  (carousel-deconvert))
(transient-call! carousel-expand ()
  ""
  :key "e"
  :interactive t
  :desc (macro-tools--transient-simple-fmt "Expand Focus" "e")
  :transient nil
  #'carousel-expand-focus)
(transient-call! carousel-reset ()
  ""
  :key "r"
  :desc (macro-tools--transient-simple-fmt "Reset" "r")
  :transient nil
  (carousel-reset-columns))
(transient-call! carousel-clear ()
  ""
  :key "K"
  :desc (macro-tools--transient-simple-fmt "Clear Carousel" "K")
  :transient nil
  (carousel-clear-ring))
(transient-call! carousel-add-buffer ()
  ""
  :key "a"
  :desc (macro-tools--transient-simple-fmt "Add" "a")
  :transient nil
  (carousel-add-current-buffer))
(transient-call! carousel-remove ()
  ""
  :key "x"
  :desc (macro-tools--transient-simple-fmt "Remove" "x")
  :transient nil
  (carousel-remove-buffer))
(transient-call! carousel-move-left ()
  ""
  :key "["
  :desc (macro-tools--transient-simple-fmt "Move Left" "[")
  (carousel-move-buffer-left))
(transient-call! carousel-move-right ()
  ""
  :key "]"
  :desc (macro-tools--transient-simple-fmt "Move Right" "]")
  (carousel-move-buffer-right))
(transient-call! carousel-claim ()
  ""
  :key "w"
  :desc (macro-tools--transient-simple-fmt "Claim Window" "w")
  (carousel-claim-window))
(transient-call! carousel-goto ()
  ""
  :key "f"
  :interactive t
  :desc (macro-tools--transient-simple-fmt "Find-Buffer" "f")
  :transient nil
  #'carousel-goto-choice)

(defun jg-workspace-carousel-title ()
  (format "Carousel: %s (%s)"
          (fmt-as-bool! (persp-parameter 'carousel))
          (if (persp-parameter 'carousel)
              (carousel--length (persp-parameter 'carousel-actual))
            "0")
          )

  )

;;;###autoload
(defun jg-workspace-build-carousel-transient ()
  (transient-define-prefix transient-carousel ()
    ""
    [:description jg-workspace-carousel-title
                  (transient-macro-call-carousel-toggle)
                  (transient-macro-call-carousel-quit)
                  (transient-macro-call-carousel-new)
                  (transient-macro-call-carousel-reset)
                  ]
    [(transient-macro-call-carousel-toggle-loop)
     (transient-macro-call-carousel-clear)
     ]
    ["Order Change"
     (transient-macro-call-carousel-move-left)
     (transient-macro-call-carousel-move-right)
     ]
    [
     ["Buffer Control "
      (transient-macro-call-carousel-claim)
      (transient-macro-call-carousel-expand)
      (transient-macro-call-carousel-goto)
      " "
      (transient-macro-call-carousel-add-buffer)
      (transient-macro-call-carousel-remove)
      " "
      (transient-macro-call-carousel-edit)
      (transient-macro-call-carousel-print)
      ]
     ]
    macro-tools--transient-quit!
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
