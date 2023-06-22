;;; window-ring--control.el -*- lexical-binding: t; -*-
(eval-when-compile
  (require 'window-ring--macros))

(defun window-ring-clear-ring (&optional arg)
  (interactive "p")
  (with-window-ring
      (message "Clearing Window Ring")
    (modify-persp-parameters `((window-ring-actual . ,(make-ring 1))
                               (window-ring-focus . 0)
                               ) wr-persp)
    )
  (window-ring-add-current-buffer arg)
  )

(defun window-ring-pop-buffers (&optional arg)
  (interactive "p")
  (with-window-ring
      ;;Pop everything from most-recent to here
      (let ((most-recent (-partial #'window-ring--get wr-actual 0))
            (target (window-ring--get wr-actual wr-focus))
            )
        (while (and (ring-length wr-actual)
                    (not (eq (funcall most-recent) target)))
          (ring-remove wr-actual 0))
        )
    (ring-resize wr-actual (ring-length wr-actual))
    )
  (when arg (window-ring-redisplay))
  )

(defun window-ring-remove-buffer (&optional buffer arg)
  " Remove the current buffer from the ring "
  (interactive "b\np")
  (with-window-ring
      (let* ((buff (if (bufferp buffer) buffer (current-buffer)))
             (index (unless (ring-empty-p wr-actual)
                      (ring-member wr-actual buff)))
             )
        (when index
          (ring-remove wr-actual index)
          (ring-resize wr-actual (ring-length wr-actual))
          )
        )
    )
  (when arg (window-ring-redisplay))
  )

(defun window-ring-replace-buffer(&optional buffer arg)
  " Replace the current focus buffer with the current buffer "
  (interactive "b\np")
  ;; TODO use with-other-window-ring
  (with-window-ring
      (let* ((buff (if (bufferp buffer) buffer (current-buffer)))
             (empty-p (ring-empty-p wr-actual))
             (index (unless empty-p (ring-member wr-actual buff)))
             (centre (unless empty-p (window-ring--get wr-actual wr-focus)))
             (ring-list (unless empty-p (ring-elements wr-actual)))
             )
        (when index
          (setf (nth (cl-position centre ring-list) ring-list) buff)
          (modify-persp-parameters `((window-ring-actual .
                                      ,(ring-convert-sequence-to-ring ring-list)))
                                   )
          )
        )
    )
  (when arg (window-ring-redisplay))
  )

(defun window-ring-add-current-buffer (&optional arg)
  (interactive "p")
  (when (and (persp-parameter 'window-ring)
             (or (buffer-local-boundp 'window-ring-buffer (current-buffer))
                 (funcall window-ring-buffer-test-fn (current-buffer)))
             (not window-ring-suppress-adding)
             )
    (window-ring-add-to-head (current-buffer) arg)
    )
  )

(defun window-ring-add-to-head (buffer &optional arg)
  (interactive "b\np")
  (with-window-ring
      (-when-let (buff (get-buffer buffer))
        (message "Adding buffer to window ring: %s" buffer)
        (ring-insert+extend wr-actual buff t)
        )
    )
  (when arg (window-ring-redisplay))
  )

(defun window-ring-add-to-tail (buffer &optional arg)
  (interactive "b\np")
  (with-window-ring
      (-when-let (buff (get-buffer buffer))
        (ring-insert-at-beginning wr-actual buffer))
    )
  (when arg (window-ring-redisplay))
  )

(defun window-ring-shrink-sides (amt)
  (interactive "NShrink By: ")
  (with-window-ring
      (let ((curr (selected-window)))
        (walk-windows #'(lambda (wind)
                          (when (not (eq wind curr))
                            (with-selected-window wind
                              (shrink-window-horizontally amt))))
                      )))
  )

(provide 'window-ring--control)
