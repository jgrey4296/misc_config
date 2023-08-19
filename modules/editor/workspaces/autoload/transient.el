;;; transient.el -*- lexical-binding: t; -*-
(require 'transient)

(progn
  (transient-make-toggle! hide-mode-line-mode)
  (transient-make-call!   quickscope
                          (format "")

                          )
  )

(transient-define-prefix jg-transient-workspace ()
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


(transient-define-prefix jg-transient-carousel ()
  ""
  [["|Carousel: %-10(persp-parameter 'carousel)"
    ("n" jg-transient-call-carousel-new)
    ("c" jg-transient-call-carousel-convert)
    ("d" jg-transient-call-carousel-deconvert)
    ("K" jg-transient-call-carousel-clear)
    ("r" jg-transient-call-carousel-reset)
    ("p" jg-transient-call-carousel-print)
    ("E" jg-transient-call-carousel-edit)
    ]
   [
    "|Window Claimed: %-10(window-parameter (selected-window) 'carousel-claimed)"
    ("l" jg-transient-call-carousel-loop)
    ("e" jg-transient-call-carousel-expand)
    ("a" jg-transient-call-carousel-add)
    ("R" jg-transient-call-carousel-remove)
    ("c" jg-transient-call-carousel-choose)
    ]
   [
    ("[" jg-transient-call-carousel-move-left)
    ("]" jg-transient-call-carousel-move-right)
    ("w" jg-transient-call-carousel-claim-window)
   ]
  ]
  transient-quit!
  )
