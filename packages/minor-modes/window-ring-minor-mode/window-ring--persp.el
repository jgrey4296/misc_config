;;; window-ring--persp.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'window-ring--macros))

(defun window-ring-new ()
  " create a new perspective and ring "
  (interactive)
  (message "Creating new window ring")
  (let ((ring-name (format "%s%s" (read-string "New Ring: ") window-ring-name-suffix))
        (curr (current-buffer))
        )
    (with-window-ring-adding
     (persp-add-new ring-name)
     (persp-switch ring-name)
     (add-hook 'find-file-hook              #'window-ring-add-current-buffer)
     (add-hook 'kill-buffer-hook            #'window-ring-remove-buffer)
     (add-hook 'kill-buffer-query-functions #'window-ring-protect-scratch-p -50)
     (switch-to-buffer (persp-parameter 'window-ring-scratch))
     (window-ring-add-to-head curr)
     (window-ring-reset-columns t)
     )
    )
  )

(defun window-ring-convert ()
  " Convert a perspective to a window ring"
  (interactive)
  (unless (persp-parameter 'window-ring (get-current-persp))
    (with-window-ring-adding
     (window-ring-create-persp-fn (get-current-persp) nil)
     (add-hook 'find-file-hook              #'window-ring-add-current-buffer)
     (add-hook 'kill-buffer-hook            #'window-ring-remove-buffer)
     (add-hook 'kill-buffer-query-functions #'window-ring-protect-scratch-p -50)
     (persp-rename (format "%s%s" (persp-name (get-current-persp)) window-ring-name-suffix))
     (window-ring-add-to-head (current-buffer))
     (window-ring-reset-columns t)
     )
    )
  )

(defun window-ring-deconvert ()
  (interactive)
  (when (persp-parameter 'window-ring (get-current-persp))
     (remove-hook 'find-file-hook              #'window-ring-add-current-buffer)
     (remove-hook 'kill-buffer-hook            #'window-ring-remove-buffer)
     (remove-hook 'kill-buffer-query-functions #'window-ring-protect-scratch-p -50)
     (mapc #'delete-persp-parameter
             '(window-ring window-ring-actual window-ring-grow window-ring-loop window-ring-duplicates
               window-ring-focus window-ring-max window-ring-backgrounds window-ring-scratch))
     (persp-delete-other-windows)
     (persp-rename (string-replace window-ring-name-suffix "" (persp-name (get-current-persp))))
     )
  )

(defun window-ring-create-persp-fn (persp hash)
  (message "Initializing window ring %s" window-ring--adding)
  (when window-ring--adding
    (modify-persp-parameters `((window-ring . t)
                               (window-ring-actual . ,(make-ring 1))
                               (window-ring-grow . t)
                               (window-ring-loop . t)
                               (window-ring-duplicates . t)
                               (window-ring-focus . 0)
                               (window-ring-max . -1)
                               (window-ring-backgrounds . ("gray19" "gray12" "gray4"))
                               (window-ring-scratch . ,(get-buffer-create (format "*%s*" (safe-persp-name persp))))
                               )
                             persp
                             )
    (ring-insert+extend (persp-parameter 'window-ring-actual persp)
                        (persp-parameter 'window-ring-scratch persp))
    )
  )

(defun window-ring-activate-persp-fn (type)
  (when (persp-parameter 'window-ring)
    (cond ('frame)
          ('window

           )
          )
    )
  )

(defun window-ring-deactivate-persp-fn (type)
  (when (persp-parameter 'window-ring)
    (cond ('frame)
          ('window)
          )
    )
  )

(provide 'window-ring--persp)
