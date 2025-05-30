;;; hooks.el -*- lexical-binding: t; -*-

(defvar +modeline--old-bar-height nil)
(defvar-local jg-dired-marked-count nil)

;;;###autoload
(defun +indent-guides-disable-maybe-h ()
  (and highlight-indent-guides-mode
       (bound-and-true-p org-indent-mode)
       (highlight-indent-guides-mode -1))
  )

;;;###autoload
(defun +modeline-hide-in-non-status-buffer-h ()
  "Show minimal modeline in magit-status buffer, no modeline elsewhere."
  (if (eq major-mode 'magit-status-mode)
      (doom-modeline-set-modeline 'magit)
    (hide-mode-line-mode))
  )

;;;###autoload
(defun +modeline-resize-for-font-h ()
  "Adjust the modeline's height when the font size is changed by
`doom/increase-font-size' or `doom/decrease-font-size'.

Meant for `doom-change-font-size-hook'."
  (unless +modeline--old-bar-height
    (setq +modeline--old-bar-height doom-modeline-height))
  (let ((default-height +modeline--old-bar-height)
        (scale (or (frame-parameter nil 'font-scale) 0)))
    (setq doom-modeline-height
          (if (> scale 0)
              (+ default-height (* scale doom-font-increment))
            default-height))))

;;;###autoload
(defun +modeline-update-env-in-all-windows-h (&rest _)
  "Update version strings in all buffers."
  (dolist (window (window-list))
    (with-selected-window window
      (when (fboundp 'doom-modeline-update-env)
        (doom-modeline-update-env))
      (force-mode-line-update))))

;;;###autoload
(defun +modeline-clear-env-in-all-windows-h (&rest _)
  "Blank out version strings in all buffers."
  (unless (modulep! +light)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (setq doom-modeline-env--version
              (bound-and-true-p doom-modeline-load-string)))))
  (force-mode-line-update t))

;;;###autoload
(defun jg-ui-modeline-update-marked-count-h (wind)
  "A hook for post-command-hook to update the count"
  (when (eq major-mode 'dired-mode)
    (let* ((marked (dired-get-marked-files nil nil nil t))
           (len (cond ((eq (car-safe marked) t) 1)
                      ((eq (length marked) 1) nil)
                      (t (length marked))))
           )
      (setq-local jg-dired-marked-count len)
      )
    )
  t
  )
