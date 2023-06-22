;;; window-ring--edit.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'window-ring--macros))

(defun window-ring-edit-order ()
  (interactive)
  (with-window-ring
      (let ((buffers (ring-elements wr-actual))
            (edit-buffer (get-buffer-create (format "*WR Buffers: %s*" (persp-parameter 'name))))
            )
        (with-current-buffer edit-buffer
          (auto-save-mode -1)
          (window-ring-edit-minor-mode 1)
          (set (make-local-variable 'backup-inhibited) t)
          (erase-buffer)
          (mapc (lambda (x) (insert (format "%s\n" (buffer-name x))))
                (reverse buffers))
          )
        (display-buffer edit-buffer)
        )
    )
  )

(defun window-ring-edit-commit ()
  (interactive)
  (with-window-ring
      (let ((order (s-split "\n" (buffer-substring-no-properties
                                  (point-min) (point-max)) t)))
        (modify-persp-parameters `((window-ring-actual . ,(make-ring 1))
                                   (window-ring-focus . 0))
                                 )
        (cl-loop for name in order
                 when (get-buffer (string-trim name))
                 do
                 (window-ring-add-to-head (get-buffer (string-trim name)))
                 )
        (kill-buffer-and-window)
        )
    )
  (window-ring-redisplay)
  )

(setq window-ring-edit-map (make-sparse-keymap))

(define-key window-ring-edit-map (kbd "C-c C-c") #'window-ring-edit-commit)

(define-minor-mode window-ring-edit-minor-mode
  " A Minor mode to commit changes to the order of window ring buffers "
  :lighter "Window-Ring-Edit"
  :keymap window-ring-edit-map
  )

(provide 'window-ring--edit)
