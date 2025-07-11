;; advice.el -*- lexical-binding: t; -*-

;;;###autoload
(defun doom-set-jump-a (fn &rest args)
  "Set a jump point and ensure fn doesn't set any new jump points."
  (better-jumper-set-jump (if (markerp (car args)) (car args)))
  (let ((evil--jumps-jumping t)
        (better-jumper--jumping t))
    (apply fn args)))

;;;###autoload
(defun doom-set-jump-maybe-a (fn &rest args)
    "Set a jump point if fn actually moves the point."
    (let ((origin (point-marker))
          (result
           (let* ((evil--jumps-jumping t)
                  (better-jumper--jumping t))
             (apply fn args)))
          (dest (point-marker)))
      (unless (equal origin dest)
        (with-current-buffer (marker-buffer origin)
          (better-jumper-set-jump
           (if (markerp (car args))
               (car args)
             origin))))
      (set-marker origin nil)
      (set-marker dest nil)
      result))

;;;###autoload
(defun jg-buffer-nav-fold-check-a (fn &rest args)
  " if at the end of the line and immediately backward theres an invisible overlay,
move back and unfold that
"
  (when-let ((eol (eolp))
             (overlay (car-safe (overlays-at (1- (point)))))
             (invis (plist-get (overlay-properties overlay) 'invisible))
             )
    (backward-char)
    )
  (apply fn args)
  )
