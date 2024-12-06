;;; carousel-transient.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header
(require 'transient)

(progn
  (transient-make-call! carousel-print       "p"
                        (transient-simple-formatter "Print Order" "p")
                        (carousel-print-order))
  (transient-make-call! carousel-edit        "E"
                        (transient-simple-formatter "Edit Order" "E")
                        :transient nil (carousel-edit-order))
  (transient-make-call! carousel-toggle-loop "l"
                        (transient-title-var-formatter "Looping" (persp-parameter 'carousel-loop) "l")
                        (carousel-toggle-loop))
  (transient-make-call! carousel-new         "n"
                        (transient-simple-formatter "New Carousel" "n")
                        :transient nil
                        (carousel-new))
  (transient-make-call! carousel-toggle      "t"
                        (transient-simple-formatter "Toggle" "t")
                        (carousel-toggle))
  (transient-make-call! carousel-quit        "T"
                        (transient-simple-formatter "Quit" "T")
                        (carousel-deconvert))
  (transient-make-int-call! carousel-expand      "e"
                        (transient-simple-formatter "Expand Focus" "e")
                        :transient nil
                        #'carousel-expand-focus)
  (transient-make-call! carousel-reset       "r"
                        (transient-simple-formatter "Reset" "r")
                        :transient nil
                        (carousel-reset-columns))
  (transient-make-call! carousel-clear       "K"
                        (transient-simple-formatter "Clear Carousel" "K")
                        :transient nil
                        (carousel-clear-ring))
  (transient-make-call! carousel-add-buffer  "a"
                        (transient-simple-formatter "Add" "a")
                        :transient nil
                        (carousel-add-current-buffer))
  (transient-make-call! carousel-remove      "x"
                        (transient-simple-formatter "Remove" "x")
                        :transient nil
                        (carousel-remove-buffer))
  (transient-make-call! carousel-move-left   "["
                        (transient-simple-formatter "Move Left" "[")
                        (carousel-move-buffer-left))
  (transient-make-call! carousel-move-right  "]"
                        (transient-simple-formatter "Move Right" "]")
                        (carousel-move-buffer-right))
  (transient-make-call! carousel-claim       "w"
                        (transient-simple-formatter "Claim Window" "w")
                        (carousel-claim-window))
  (transient-make-int-call! carousel-goto        "f"
                            (transient-simple-formatter "Find-Buffer" "f")
                            :transient nil
                            #'carousel-goto-choice)
  )

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
     [(transient-macro-call-carousel-toggle)
      (transient-macro-call-carousel-quit)
      (transient-macro-call-carousel-new)
      (transient-macro-call-carousel-reset)
      ]
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
