;;; auto-modes.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-default-debug-buffer-state ()
  (interactive)
  (let* ((name (file-name-sans-versions (or (buffer-file-name) (buffer-name))))
         (result (assoc-default name auto-mode-alist 'string-match))
        )
    (message (string-join
              (list
               (format"Buffer %s (%s)" (buffer-name) major-mode)
               (format "Auto-Mode: %s" result)
               (format "(Dedicated %s) (popup %s) (Live %s) (Real %s)"
                       (---truthy? (window-dedicated-p))
                       (---truthy? (+popup-buffer-p))
                       (buffer-live-p (current-buffer))
                       (---truthy? (doom-real-buffer-p (current-buffer)))
                       )
               (format "(Modified %s) (in-project %s)"
                       (---truthy? (buffer-modified-p (current-buffer)))
                       (and (projectile-project-root)
                            (f-ancestor-of? (projectile-project-root)
                                            (buffer-file-name)))
                       )
               )
              "\n"
              )
             )
    )
  )
