;;; carousel-transient.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header
(require 'transient)

(transient-call! carousel-print ()
  ""
  :key "p"
  :desc (transient-simple-formatter "Print Order" "p")
  (carousel-print-order))
(transient-call! carousel-edit ()
  ""
  :key "E"
  :desc (transient-simple-formatter "Edit Order" "E")
  :transient nil (carousel-edit-order))
(transient-call! carousel-toggle-loop ()
  ""
  :key "l"
  :desc (transient-var-fmt "Looping" (persp-parameter 'carousel-loop) "l")
  (carousel-toggle-loop))
(transient-call! carousel-new ()
  ""
  :key "n"
  :desc (transient-simple-formatter "New Carousel" "n")
  :transient nil
  (carousel-new))
(transient-call! carousel-toggle ()
  ""
  :key "t"
  :desc (transient-simple-formatter "Toggle" "t")
  (carousel-toggle))
(transient-call! carousel-quit ()
  ""
  :key "T"
  :desc (transient-simple-formatter "Quit" "T")
  (carousel-deconvert))
(transient-call! carousel-expand ()
  ""
  :key "e"
  :interactive t
  :desc (transient-simple-formatter "Expand Focus" "e")
  :transient nil
  #'carousel-expand-focus)
(transient-call! carousel-reset ()
  ""
  :key "r"
  :desc (transient-simple-formatter "Reset" "r")
  :transient nil
  (carousel-reset-columns))
(transient-call! carousel-clear ()
  ""
  :key "K"
  :desc (transient-simple-formatter "Clear Carousel" "K")
  :transient nil
  (carousel-clear-ring))
(transient-call! carousel-add-buffer ()
  ""
  :key "a"
  :desc (transient-simple-formatter "Add" "a")
  :transient nil
  (carousel-add-current-buffer))
(transient-call! carousel-remove ()
  ""
  :key "x"
  :desc (transient-simple-formatter "Remove" "x")
  :transient nil
  (carousel-remove-buffer))
(transient-call! carousel-move-left ()
  ""
  :key "["
  :desc (transient-simple-formatter "Move Left" "[")
  (carousel-move-buffer-left))
(transient-call! carousel-move-right ()
  ""
  :key "]"
  :desc (transient-simple-formatter "Move Right" "]")
  (carousel-move-buffer-right))
(transient-call! carousel-claim ()
  ""
  :key "w"
  :desc (transient-simple-formatter "Claim Window" "w")
  (carousel-claim-window))
(transient-call! carousel-goto ()
  ""
  :key "f"
  :interactive t
  :desc (transient-simple-formatter "Find-Buffer" "f")
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
