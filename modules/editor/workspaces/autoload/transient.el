;;; transient.el -*- lexical-binding: t; -*-
(require 'transient)

(progn
  (transient-make-mode-toggle! hide-mode-line-mode)
  (transient-make-call!   quickscope
                          (format "")

                          )
  )

(transient-define-prefix transient-workspace ()
  ""
  []
  [""

   ]
  transient-quit!
  )


(progn
  (transient-make-call! carousel-pause-redisplay
                        (format "%-2s : Pause Redisplay" (fmt-as-bool! carousel-pause-auto-redisplay))
                        (setq carousel-pause-auto-redisplay (not carousel-pause-auto-redisplay))
                        )
  "p" carousel-print-order
  "E" carousel-edit-order
  "l" carousel-toggle-loop
  "n" carousel-new
  "c" carousel-convert
  "d" carousel-deconvert
  "e" carousel-shrink-sides
  "r" carousel-reset-columns
  "K" carousel-clear-ring
  "a" carousel-add-current-buffer
  "R" carousel-remove-buffer
  "[" carousel-move-buffer-left
  "]" carousel-move-buffer-right
  "w" carousel-claim-window

  )


(transient-define-prefix transient-carousel ()
  ""
  [["|Carousel: %-10(persp-parameter 'carousel)"
    ("n" transient-macro-call-carousel-new)
    ("c" transient-macro-call-carousel-convert)
    ("d" transient-macro-call-carousel-deconvert)
    ("K" transient-macro-call-carousel-clear)
    ("r" transient-macro-call-carousel-reset)
    ("p" transient-macro-call-carousel-print)
    ("E" transient-macro-call-carousel-edit)
    ]
   [
    "|Window Claimed: %-10(window-parameter (selected-window) 'carousel-claimed)"
    ("l" transient-macro-call-carousel-loop)
    ("e" transient-macro-call-carousel-expand)
    ("a" transient-macro-call-carousel-add)
    ("R" transient-macro-call-carousel-remove)
    ("c" transient-macro-call-carousel-choose)
    ]
   [
    ("[" transient-macro-call-carousel-move-left)
    ("]" transient-macro-call-carousel-move-right)
    ("w" transient-macro-call-carousel-claim-window)
   ]
  ]
  transient-quit!
  )
