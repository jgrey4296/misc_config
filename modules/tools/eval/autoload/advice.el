;;; advice.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +eval--quickrun-fix-evil-visual-region-a ()
  "Make `quickrun-replace-region' recognize evil visual selections."

  (let ((output (buffer-substring-no-properties (point-min) (point-max))))
    (with-current-buffer quickrun--original-buffer
      (cl-destructuring-bind (beg . end)
          ;; Because `deactivate-mark', the function, was used in
          ;; `quickrun--region-command-common' instead of `deactivate-mark',
          ;; the variable, the selection is disabled by this point.
          (if (bound-and-true-p evil-local-mode)
              (cons evil-visual-beginning evil-visual-end)
            (cons (region-beginning) (region-end)))
        (delete-region beg end)
        (insert output))
      (setq quickrun-option-outputter quickrun--original-outputter))))

;;;###autoload
(defun +eval--quickrun-auto-close-a (&rest _)
  "Silently re-create the quickrun popup when re-evaluating."

  (when-let (win (get-buffer-window quickrun--buffer-name))
    (let ((inhibit-message t))
      (quickrun--kill-running-process)
      (message ""))
    (delete-window win)))

;;;###autoload
(defun +eval--show-output-in-overlay-a (fn)
  (lambda (process event)
    (funcall fn process event)
    (with-current-buffer quickrun--buffer-name
      (when (> (buffer-size) 0)
        (+eval-display-results
         (string-trim (buffer-string))
         quickrun--original-buffer)))))

;;;###autoload
(defun +eval--inhibit-quickrun-popup-a (buf cb)
  (setq quickrun--original-buffer (current-buffer))
  (save-window-excursion
    (with-current-buffer (pop-to-buffer buf)
      (setq quickrun-option-outputter #'ignore)
      (funcall cb))))

;;;###autoload
(defun +eval--show-output-in-overlay-a (fn)
  (lambda (process event)
    (funcall fn process event)
    (with-current-buffer quickrun--buffer-name
      (when (> (buffer-size) 0)
        (+eval-display-results
         (string-trim (buffer-string))
         quickrun--original-buffer)))))

;;;###autoload
(defun +eval--inhibit-quickrun-popup-a (buf cb)
  ;; Suppress quickrun's popup window because we're using an overlay instead.
  (setq quickrun--original-buffer (current-buffer))
  (save-window-excursion
    (with-current-buffer (pop-to-buffer buf)
      (setq quickrun-option-outputter #'ignore)
      (funcall cb))))
