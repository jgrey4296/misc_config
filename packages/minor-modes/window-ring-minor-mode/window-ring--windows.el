;;; window-ring--windows.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'window-ring--macros))

(defun window-ring-reset-columns (&optional arg)
  (interactive "p")
  (window-ring-setup-columns arg t)
  )

(defun window-ring-setup-columns (&optional arg soft)
  " Reset windows using `window-ring-column-fn`
    if SOFT then don't clear the window ring "
  (interactive "pi")
  (persp-delete-other-windows)
  (mapc (-rpartial #'window-ring-claim-window t) (funcall window-ring-column-fn arg soft))

  (unless soft
    ;; clear ring
    (modify-persp-parameters `((window-ring-actual . ,(make-ring 1))))
    (window-ring-add-current-buffer)
    )

  (when arg
    (window-ring-redisplay))
  )

(defun window-ring-setup-columns-default (&optional arg soft)
  ;; (arg == 1 -> one row) (else -> two rows, only use top)
  ;; Clear
  (let ((leftmost (selected-window))
        centre rightmost)
    ;; split
    (when (and (numberp arg) (< 1 arg)) ;; Split as top 3 columns
      (split-window-below))

    ;; Split as 3 columns
    (setq centre (split-window-right))
    (select-window centre)
    (setq rightmost (split-window-right))

    (when (and arg evil-auto-balance-windows)
      (balance-windows))

    (list leftmost centre rightmost)
    )
  )

(defun window-ring-setup-vertical (&optional arg soft)
  (let ((top (selected-window))
        middle
        bottom)
    (setq middle (split-window-below))
    (select-window middle)
    (setq bottom (split-window-below))

    (when (and arg evil-auto-balance-windows)
      (balance-windows))
    (list top middle bottom)
    )
  )

(defun window-ring-kill-persp-fn (persp)
  (with-other-window-ring persp
      (message "Killing Window Ring")
    )
  )

(defun window-ring-claim-window (&optional wind force)
  "Claim a window as one suitable for showing ring buffers. returns the window"
  (interactive)
  (let ((wind (or wind (selected-window))))
    (when-window-ring
        (set-window-parameter wind 'window-ring-claimed
                              (if force t
                                (not (window-parameter wind 'window-ring-claimed))))
      )
    wind
    )
  )

(defun window-ring-redisplay ()
  (interactive)
  (when-window-ring
      (pcase window-ring-focus-style
        ('balanced
         (window-ring--redisplay-balanced))
        ('newest
        (window-ring--redisplay-newest))
        ('oldest
         (window-ring--redisplay-oldest))
        (_ (message "Unknown redisplay style"))
        )
    )
  )

(defun window-ring--redisplay-balanced ()
  (with-window-ring
      (let* ((largest  (window-ring--rough-mid (window-at-side-list nil window-ring-selector)))
             (ring-len (ring-length wr-actual))
             (curr-win (window-next-sibling largest))
             (index    (window-ring--newer ring-len wr-focus wr-loop))
             )
        (message "Windows: Largest: %s (%s) curr: %s (%s)" largest (window-live-p largest) curr-win (window-live-p curr-win))
        (message "Window-ring-state: (%s) %s" ring-len wr-actual)
        (set-window-buffer largest (window-ring--get wr-actual wr-focus))
        (while curr-win
          (setq index (if (window-ring-set-window curr-win index) (window-ring--newer ring-len index wr-loop) index)
                curr-win (window-next-sibling curr-win))
        )
        (setq index (window-ring--older ring-len wr-focus wr-loop)
              curr-win (window-prev-sibling largest))
        (while curr-win
          (setq index (if (window-ring-set-window curr-win index) (window-ring--older ring-len index wr-loop) index)
                curr-win (window-prev-sibling curr-win))
          )
        )
    )
  )

(defun window-ring--redisplay-newest ()
  (with-window-ring
      (let ((windows (reverse (window-at-side-list nil window-ring-selector)))
            (ring-len (ring-length wr-actual))
            (focus wr-focus)
            )
        (while windows
          (if (window-ring-set-window (pop windows) focus)
              (setq focus (window-ring--older ring-len focus wr-loop)))
          )
        )
    )
  )

(defun window-ring--redisplay-oldest ()
  (with-window-ring
      (let ((windows (window-at-side-list nil window-ring-selector))
            (ring-len (ring-length wr-actual))
            (focus wr-focus)
            )
        (while windows
          (if (window-ring-set-window (pop windows) focus)
              (setq focus (window-ring--newer ring-len focus wr-loop)))
          )
        )
    )
  )

(defun window-ring-set-window (window index)
  (with-window-ring
      (unless (window-live-p window) (select-window window))
    (set-window-buffer window (if index (window-ring--get wr-actual index) wr-scratch))
    (if (window-parameter window 'window-ring-claimed)
        (message "Unclaimed Window: %s" window)
      t
      )
    )
  )

(provide 'window-ring--windows)
