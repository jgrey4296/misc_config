;;; editor/window-control/+funcs.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-misc-modify-line-end-display-table ()
  (interactive)
  " from https://stackoverflow.com/questions/8370778/ "
  ;; Modify the display table for whitespace, so lines which
  ;; truncate are not signaled with a $
  (set-display-table-slot standard-display-table 0 ?\ )
  )

;;;###autoload
(defun +jg-ui-list-frames ()
  (interactive)
  (with-temp-buffer-window "*Frames*" 'display-buffer-pop-up-window nil
    (cl-loop for f in (frame-list)
             do
             (princ f)
             (princ "\n")
             )
    (princ (format "Current Frame: %s" (selected-frame)))
    )
  )

;;;###autoload
(defun +jg-ui-cleanup-frames ()
  (interactive)
  (cl-loop for f in (frame-list)
           do
           (if (not (equal f (selected-frame)))
               (delete-frame f)
               )
           )
  )

;;;###autoload
(defun +jg-ui-refresh-highlighting (&optional window)
  (interactive)
  (let* ((buff (window-buffer window))
        (multi-windows (get-buffer-window-list buff))
        (is-first (or (eq (length multi-windows) 1)
                      (eq window (car multi-windows))
                      )
                  )
        ;; (vis-min (save-excursion (with-selected-window (or window (selected-window))
        ;;                            (evil-window-top) (point))))
        ;; (vis-max (save-excursion (with-selected-window (or window (selected-window))
        ;;                            (evil-window-bottom) (point))))
        )
    (when (and (not (minibufferp))
               (equal buff (current-buffer))
               is-first
               )
      (when font-lock-mode
        ;; (message "Triggering Font Lock Update")
        (font-lock-update)
        )
      (when (and (boundp 'treesit-font-lock-settings) treesit-font-lock-settings)
        ;; (message "Triggering Treesit update")
        (treesit-font-lock-fontify-region (point-min) (point-max))
        )
      (when tree-sitter-hl-mode
        ;; (message "Triggering Treesitter hl update")
        (tree-sitter-hl--highlight-region (point-min) (point-max))
         )
        )
      )
    )
