;;; window-ring--movement.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'window-ring--macros))

(defun window-ring-toggle-loop ()
  (interactive)
  (with-window-ring
      (modify-persp-parameters `((window-ring-loop . ,(not wr-loop))))
    )
  )

(defun window-ring-move-focus (&optional arg)
  " move the focus towards the most recent addition to window ring.
 if arg is not nil, move towards oldest "
  (interactive "p")
  (cond ((persp-parameter 'window-ring)
         (let* ((wr-persp      (get-current-persp))
                (wr-actual     (persp-parameter 'window-ring-actual))
                (wr-grow       (persp-parameter 'window-ring-grow))
                (wr-loop       (persp-parameter 'window-ring-loop))
                (wr-duplicates (persp-parameter 'window-ring-duplicates))
                (curr-focus    (ring-member wr-actual (current-buffer)))
                (wr-focus      (persp-parameter 'window-ring-focus))
                (wr-max        (persp-parameter 'window-ring-max))
                (wr-scratch    (persp-parameter 'window-ring-scratch))
                (new-focus (if (< 1 arg)
                               (window-ring--older (ring-length wr-actual) wr-focus wr-loop)
                            (window-ring--newer (ring-length wr-actual) wr-focus wr-loop)))
                )
           (when new-focus
             (modify-persp-parameters `((window-ring-focus . ,new-focus)))
             )
           )
         (window-ring-redisplay)
         (window-ring-print-order)
         )
        ((< 1 arg)
         (evil-window-left 1))
        (t
         (evil-window-right 1))
        )
  )

(defun window-ring-move-focus-alt ()
  (interactive)
  (window-ring-move-focus 2)
  )

(defun window-ring-goto-newest (&optional arg)
  (interactive "p")
  (with-window-ring
      (modify-persp-parameters '((window-ring-focus . 0)))
    (when arg (window-ring-redisplay))
    )
  )

(defun window-ring-goto-oldest (&optional arg)
  (interactive "p")
  (with-window-ring
      (modify-persp-parameters `((window-ring-focus . ,(1- (ring-length wr-actual)))))
    (when arg (window-ring-redisplay))
    )
  )

(defun window-ring-move-buffer-right (&optional arg)
  (interactive "p")
  (with-window-ring
      (let* ((curr-focus (ring-member wr-actual (current-buffer)))
             (curr (window-ring--get wr-actual curr-focus))
             (next (window-ring--get wr-actual (window-ring--newer (ring-length wr-actual) curr-focus wr-loop)))
             (as-seq (ring-elements wr-actual))
             new-ring
            )
        (setq new-ring (ring-convert-sequence-to-ring (cl-loop for val in as-seq
                                                               collect (cond ((eq val curr)
                                                                              next)
                                                                             ((eq val next)
                                                                              curr)
                                                                             (t val))
                                                               )))
      (modify-persp-parameters `((window-ring-actual . ,new-ring)))
      (modify-persp-parameters `((window-ring-focus . ,(ring-member new-ring curr))))
      )
    (when arg (window-ring-redisplay))
    (select-window (get-buffer-window (current-buffer)))
    )
  (window-ring-print-order)
  )

(defun window-ring-move-buffer-left (&optional arg)
  (interactive "p")
  (with-window-ring
      (let* ((curr-focus (ring-member wr-actual (current-buffer)))
             (curr (window-ring--get wr-actual curr-focus))
             (next (window-ring--get wr-actual (window-ring--older (ring-length wr-actual) curr-focus wr-loop)))
             (as-seq (ring-elements wr-actual))
             new-ring
            )
        (setq new-ring (ring-convert-sequence-to-ring (cl-loop for val in as-seq
                                                               collect (cond ((eq val curr)
                                                                              next)
                                                                             ((eq val next)
                                                                              curr)
                                                                             (t val))
                                                               )))
      (modify-persp-parameters `((window-ring-actual . ,new-ring)))
      (modify-persp-parameters `((window-ring-focus . ,(ring-member new-ring curr))))
      )
    (when arg (window-ring-redisplay))
    (select-window (get-buffer-window (current-buffer)))
    )
  (window-ring-print-order)
  )

(provide 'window-ring--movement)
