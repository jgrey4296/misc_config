;;; hooks.el -*- lexical-binding: t; -*-


(defun +popup--kill-buffer (buffer ttl)
  "Tries to kill BUFFER, as was requested by a transient timer. If it fails, eg.
the buffer is visible, then set another timer and try again later."
  (let ((inhibit-quit t))
    (cond ((not (buffer-live-p buffer)))
          ((not (get-buffer-window buffer t))
           (with-demoted-errors "Error killing transient buffer: %s"
             (with-current-buffer buffer
               (let ((kill-buffer-hook (remq '+popup-kill-buffer-hook-h kill-buffer-hook))
                     confirm-kill-processes)
                 (when-let (process (get-buffer-process buffer))
                   (when (eq (process-type process) 'real)
                     (kill-process process)))
                 (let (kill-buffer-query-functions)
                   ;; HACK The debugger backtrace buffer, when killed, called
                   ;;      `top-level'. This causes jumpiness when the popup
                   ;;      manager tries to clean it up.
                   (cl-letf (((symbol-function #'top-level) #'ignore))
                     (kill-buffer buffer)))))))
          ((let ((ttl (if (= ttl 0)
                          (or (plist-get +popup-defaults :ttl) 3)
                        ttl)))
             (with-current-buffer buffer
               (setq +popup--timer
                     (run-at-time ttl nil #'+popup--kill-buffer buffer ttl))))))))

(defun +popup--delete-window (window)
  "Do housekeeping before destroying a popup window.

+ Disables `+popup-buffer-mode' so that any hooks attached to it get a chance to
  run and do cleanup of its own.
+ Either kills the buffer or sets a transient timer, if the window has a
  `transient' window parameter (see `+popup-window-parameters').
+ And finally deletes the window!"
  (let ((buffer (window-buffer window))
        (inhibit-quit t))
    (and (or (buffer-file-name buffer)
             (if-let (base-buffer (buffer-base-buffer buffer))
                 (buffer-file-name base-buffer)))
         (buffer-modified-p buffer)
         (let ((autosave (+popup-parameter 'autosave window)))
           (cond ((eq autosave 't))
                 ((null autosave)
                  (y-or-n-p "Popup buffer is modified. Save it?"))
                 ((functionp autosave)
                  (funcall autosave buffer))))
         (with-current-buffer buffer (save-buffer)))
    (let ((ignore-window-parameters t))
      (if-let (wconf (window-parameter window 'saved-wconf))
          (set-window-configuration wconf)
        (delete-window window)))
    (unless (window-live-p window)
      (with-current-buffer buffer
        (set-buffer-modified-p nil)
        (+popup-buffer-mode -1)
        (unless +popup--inhibit-transient
          (let ((ttl (+popup-parameter 'ttl window)))
            (when (eq ttl 't)
              (setq ttl (plist-get +popup-defaults :ttl)))
            (cond ((null ttl))
                  ((functionp ttl)
                   (funcall ttl buffer))
                  ((not (integerp ttl))
                   (signal 'wrong-type-argument (list 'integerp ttl)))
                  ((= ttl 0)
                   (+popup--kill-buffer buffer 0))
                  ((add-hook 'kill-buffer-hook #'+popup-kill-buffer-hook-h nil t)
                   (setq +popup--timer
                         (run-at-time ttl nil #'+popup--kill-buffer
                                      buffer ttl))))))))))

(defun +popup--delete-other-windows (window)
  "Fixes `delete-other-windows' when used from a popup window."
  (when-let (window (ignore-errors (+popup/raise window)))
    (let ((ignore-window-parameters t))
      (delete-other-windows window)))
  nil)


;;;###autoload
(defun +popup-adjust-fringes-h ()
  "Hides the fringe in popup windows, restoring them if `+popup-buffer-mode' is
disabled."
  (let ((f (if (bound-and-true-p +popup-buffer-mode) 0)))
    (set-window-fringes nil f f fringes-outside-margins)))

;;;###autoload
(defun +popup-adjust-margins-h ()
  "Creates padding for the popup window determined by `+popup-margin-width',
restoring it if `+popup-buffer-mode' is disabled."
  (when +popup-margin-width
    (unless (memq (window-parameter nil 'window-side) '(left right))
      (let ((m (if (bound-and-true-p +popup-buffer-mode) +popup-margin-width)))
        (set-window-margins nil m m)))))

(defvar hide-mode-line-format)
;;;###autoload
(defun +popup-set-modeline-on-enable-h ()
  "Don't show modeline in popup windows without a `modeline' window-parameter.
Possible values for this parameter are:

  t            show the mode-line as normal
  nil          hide the modeline entirely (the default)
  a function   `mode-line-format' is set to its return value

Any non-nil value besides the above will be used as the raw value for
`mode-line-format'."
  (when (bound-and-true-p +popup-buffer-mode)
    (let ((modeline (+popup-parameter 'modeline)))
      (cond ((eq modeline 't))
            ((null modeline)
             ;; TODO use `mode-line-format' window parameter instead (emacs 26+)
             (hide-mode-line-mode +1))
            ((let ((hide-mode-line-format
                    (if (functionp modeline)
                        (funcall modeline)
                      modeline)))
               (hide-mode-line-mode +1)))))))
(put '+popup-set-modeline-on-enable-h 'permanent-local-hook t)

;;;###autoload
(defun +popup-unset-modeline-on-disable-h ()
  "Restore the modeline when `+popup-buffer-mode' is deactivated."
  (when (and (not (bound-and-true-p +popup-buffer-mode))
             (bound-and-true-p hide-mode-line-mode))
    (hide-mode-line-mode -1)))

;;;###autoload
(defun +popup-close-on-escape-h ()
  "If called inside a popup, try to close that popup window (see
`+popup/close'). If called outside, try to close all popup windows (see
`+popup/close-all')."
  (if (+popup-window-p)
      (+popup/close)
    (+popup/close-all)))

;;;###autoload
(defun +popup-kill-buffer-hook-h ()
  "TODO"
  (when-let (window (get-buffer-window))
    (when (+popup-window-p window)
      (let ((+popup--inhibit-transient t))
        (+popup--delete-window window)))))
