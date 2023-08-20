;;; hooks.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +ivy--set-jump-point-maybe-h ()
      (when (markerp (bound-and-true-p +ivy--origin))
        (unless (equal (ignore-errors (with-ivy-window (point-marker)))
                       +ivy--origin)
          (with-current-buffer (marker-buffer +ivy--origin)
            (better-jumper-set-jump +ivy--origin)))
        (set-marker +ivy--origin nil))
      (setq +ivy--origin nil)
      )
