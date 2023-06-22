;;; window-ring--util.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'window-ring--macros))

(defun window-ring--newer (len index loop)
  " given the ring length, current index, and loop param
return the next newer index "
  (pcase index
    ((pred null) nil)
    ((or (guard loop) (guard (< 0 index)))
     (ring-minus1 index len))
    (_ nil)
    )
  )

(defun window-ring--older (len index loop)
  " given the ring length, current index, and loop param,
return the next older index"
  (pcase index
    ((pred null) nil)
    ((or (guard loop) (guard (< index (1- len))))
     (ring-plus1 index len))
    (_ nil)
    )
  )

(defun window-ring--print-fn (buff focus selected)
  (message "Got: %s %s %s" buff (eq buff focus) selected)
  (cond
    ((string-equal buff focus)
     (propertize (format "<- [%s] ->" buff) 'face '(:background "blue")))
    ((string-equal buff selected)
     (propertize (format " [%s] " buff) 'face '(:background "darkgreen")))
    (t
     (format "%s" buff))
    )
  )

(defun window-ring-print-order ()
  (interactive)
  (with-window-ring
      (let ((elements (mapcar #'buffer-name (-non-nil (reverse (ring-elements wr-actual)))))
            (focus (buffer-name (window-ring--get wr-actual wr-focus)))
            (selected (buffer-name (window-buffer (selected-window))))
            )
        (message "Ring Buffers: %s"
                 (string-join (mapcar (-rpartial #'window-ring--print-fn focus selected) elements) " | "))
        )
    )
  )

(defun window-ring-print-claimed-status ()
  (interactive)
  (when-window-ring
      (let* ((largest  (get-largest-window 'visible))
             (curr-win (window-next-sibling largest))
            windows
            )
        (while curr-win
          (push (propertize (format "%s" (window-buffer curr-win))
                            'face (if (window-parameter curr-win 'window-ring-claimed)
                                      '(:background "blue")
                                    '(:background "red"))) windows)
          (setq curr-win (window-next-sibling curr-win))
          )
        (setq windows (reverse windows))
        (push (propertize (format "%s" (window-buffer largest))
                          'face (if (window-parameter curr-win 'window-ring-claimed)
                                    '(:background "blue")
                                  '(:background "red"))) windows)
        (setq curr-win (window-prev-sibling largest))
        (while curr-win
          (push (propertize (format "%s" (window-buffer curr-win))
                            'face (if (window-parameter curr-win 'window-ring-claimed)
                                      '(:background "blue")
                                    '(:background "red"))) windows)
          (setq curr-win (window-prev-sibling curr-win))
          )
        (message (format "*Claimed WR Buffers: %s*"
                         (string-join windows " | ")
                         ))
        )
        )
    )

(defun window-ring--focus-set (count)
  "get count number of buffers around the focus"
  (with-window-ring
      (let ((side-count (/ (1- count) 2))
            (curr wr-focus)
            (ring-idx #'(lambda (x)
                          (ring-index x
                                      0
                                      (ring-length wr-actual)
                                      (ring-size wr-actual))
                          ))
            buffers
            )
        (-reject #'null (cl-loop for val in (number-sequence (- wr-actual side-count) (+ wr-actual side-count))
                                 collect (window-ring--get wr-actual (funcall ring-idx val))
                                 )
                 )
        )
    )
  )

(defun window-ring-get-claimed ()
  (-filter #'window-ring-window-claimed-p (window-list))
  )

(defun window-ring--rough-mid (lst)
  (pcase (length lst)
    (0 nil)
    (1 (car lst))
    (2 (cadr lst))
    (3 (cadr lst))
    (_ (nth (/ (length lst) 2) lst))
    )
  )

(provide 'window-ring--util)
