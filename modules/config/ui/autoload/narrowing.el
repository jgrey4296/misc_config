;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-ui-narrow-around-point ()
  (interactive)
  (cond (current-prefix-arg
         (narrow-to-region (line-beginning-position)
                           (point-max)))
        ((eq evil-state 'visual)
         (narrow-to-region evil-visual-beginning evil-visual-end))
        ((not (buffer-narrowed-p))
         (let ((num (read-number "Lines Around Point to Select: ")))
           (narrow-to-region (line-beginning-position (- num))
                             (line-end-position num))
           )
         )
        (t
         (widen))
        )
  )

;;;###autoload
(defun +jg-ui-toggle-narrow-buffer (arg)
  "Narrow the buffer to BEG END. If narrowed, widen it.
If region isn't active, narrow away anything above point
"
  (interactive "P")
  (cond ((eq evil-state 'normal)
         (narrow-to-region (line-beginning-position) (point-max)))
        ((eq evil-state 'visual)
         (narrow-to-region evil-visual-beginning evil-visual-end))
        )
  )

;;;###autoload
(defun +jg-ui-narrowing-move-focus-backward (arg)
  (interactive "p")
  (+jg-ui-narrowing-move-focus-forward(- arg))
  )

;;;###autoload
(defun +jg-ui-narrowing-move-focus-forward (arg)
  (interactive "p")
  (widen)
  (evil-forward-section-begin arg)
  (let ((bounds (+evil:defun-txtobj)))
    (narrow-to-region (car bounds) (cadr bounds))
    )
  )
