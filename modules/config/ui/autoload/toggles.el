;;; toggles.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-ui-window-layout-toggle ()
  "Toggle between horizontal and vertical layout of two windows.
originally from spacemacs, then @bmag"
  (interactive)
  (let* ((windows (--reject (equal it transient--window) (window-list)))
         (window-tree (car (window-tree)))
         (current-split-vertical-p (car window-tree))
         (first-window (car windows))
         (second-window-buffer (window-buffer (cadr windows)))
         (splitter (if current-split-vertical-p
                       #'split-window-horizontally
                     #'split-window-vertically)))

    (unless (= (length windows) 2)
      (error "Can't toggle window layout when the number of windows isn't two."))

    (delete-other-windows first-window)
    (set-window-buffer (funcall splitter) second-window-buffer)
    )
  )

;;;###autoload
(defun +jg-ui-window-rotate-forward (&optional count)
  "Rotate each window forwards.
A negative prefix argument rotates each window backwards.
Dedicated (locked) windows are left untouched.
originally from spacemacs, by magnars and modified by ffevotte
"
  (interactive "p")
  (let* ((windows (--reject (or (window-dedicated-p it) (equal it transient--window)) (window-list)))
         (states (mapcar #'window-state-get windows))
         (num-windows (length windows))
         (step (+ num-windows (or count 1))))
    (unless (< 1 num-windows)
      (error "You can't rotate a single window!"))

    (dotimes (i num-windows)
      (window-state-put
       (elt states i)
       (elt windows (% (+ step i) num-windows))))
    )
  )
