;;; ui/popup/autoload/popup.el -*- lexical-binding: t; -*-

;;-- utils
(defvar +popup--internal nil)

(defun +popup--normalize-alist (alist)
  "Merge `+popup-default-alist' and `+popup-default-parameters' with ALIST."
  (when alist
    (let ((alist  ; handle defaults
           (cl-remove-duplicates
            (append alist (or +popup-default-alist nil))
            :key #'car-safe :from-end t))
          (parameters
           (cl-remove-duplicates
            (append (cdr (assq 'window-parameters alist))
                    +popup-default-parameters)
            :key #'car-safe :from-end t)))
      ;; handle `size'
      (when-let* ((size  (cdr (assq 'size alist)))
                  (side  (or (cdr (assq 'side alist)) 'bottom))
                  (param (if (memq side '(left right))
                             'window-width
                           'window-height)))
        (setq list (assq-delete-all 'size alist))
        (setf (alist-get param alist) size))
      (setf (alist-get 'window-parameters alist)
            parameters)
      ;; Fixes #1305: addresses an edge case where a popup with a :size, :width
      ;; or :height greater than the current frame's dimensions causes
      ;; hanging/freezing (a bug in Emacs' `display-buffer' API perhaps?)
      (let ((width  (cdr (assq 'window-width  alist)))
            (height (cdr (assq 'window-height alist))))
        (setf (alist-get 'window-width alist)
              (if (numberp width)
                  (min width (frame-width))
                width))
        (setf (alist-get 'window-height alist)
              (if (numberp height)
                  (min height (frame-height))
                height))
        alist))))

(defun +popup--maybe-select-window (window origin)
  "Select a window based on `+popup--inhibit-select' and this window's `select' parameter."
  (unless +popup--inhibit-select
    (let ((select (+popup-parameter 'select window)))
      (if (functionp select)
          (funcall select window origin)
        (select-window (if select window origin))))))

;;-- end utils

;;;###autoload
(defun +popup--init (window &optional alist)
  "Initializes a popup window. Run any time a popup is opened. It sets the
default window parameters for popup windows, clears leftover transient timers
and enables `+popup-buffer-mode'."
  (with-selected-window window
    (setq alist (delq (assq 'actions alist) alist))
    (set-window-parameter window 'popup t)
    (set-window-parameter window 'split-window #'+popup--split-window)
    (set-window-parameter window 'delete-window #'+popup--delete-window)
    (set-window-parameter window 'delete-other-windows #'+popup--delete-other-windows)
    (set-window-dedicated-p window 'popup)
    (window-preserve-size
     window (memq (window-parameter window 'window-side)
                  '(left right))
     t)
    (+popup-buffer-mode +1)
    (run-hooks '+popup-create-window-hook)))

;;;###autoload
(defun +popup-buffer-p (&optional buffer)
  "Return non-nil if BUFFER is a popup buffer. Defaults to the current buffer."
  (when +popup-mode
    (let ((buffer (or buffer (current-buffer))))
      (and (bufferp buffer)
           (buffer-live-p buffer)
           (buffer-local-value '+popup-buffer-mode buffer)
           buffer))))

;;;###autoload
(defun +popup-window-p (&optional window)
  "Return non-nil if WINDOW is a popup window. Defaults to the current window."
  (when +popup-mode
    (let ((window (or window (selected-window))))
      (and (windowp window)
           (window-live-p window)
           (window-parameter window 'popup)
           window))))

;;;###autoload
(defun +popup-buffer (buffer &optional alist)
  "Open BUFFER in a popup window. ALIST describes its features."
  (let* ((origin (selected-window))
         (window-min-height 3)
         (alist (+popup--normalize-alist alist))
         (actions (or (cdr (assq 'actions alist))
                      +popup-default-display-buffer-actions)))
    (or (let* ((alist (remove (assq 'window-width alist) alist))
               (alist (remove (assq 'window-height alist) alist))
               (window (display-buffer-reuse-window buffer alist)))
          (when window
            (+popup--maybe-select-window window origin)
            window))
        (when-let (popup (cl-loop for func in actions
                                  if (funcall func buffer alist)
                                  return it))
          (+popup--init popup alist)
          (+popup--maybe-select-window popup origin)
          popup))))

;;;###autoload
(defun +popup-parameter (parameter &optional window)
  "Fetch the window PARAMETER (symbol) of WINDOW"
  (window-parameter (or window (selected-window)) parameter))

;;;###autoload
(defun +popup-parameter-fn (parameter &optional window &rest args)
  "Fetch the window PARAMETER (symbol) of WINDOW. If it is a function, run it
with ARGS to get its return value."
  (let ((val (+popup-parameter parameter window)))
    (if (functionp val)
        (apply val args)
      val)))

;;;###autoload
(defun +popup-windows ()
  "Returns a list of all popup windows."
  (cl-remove-if-not #'+popup-window-p (window-list)))

;;;###autoload
(defun +popup-shrink-to-fit (&optional window)
  "Shrinks WINDOW to fit the buffer contents, if the buffer isn't empty.

Uses `shrink-window-if-larger-than-buffer'."
  (unless window
    (setq window (selected-window)))
  (unless (= (- (point-max) (point-min)) 0)
    (shrink-window-if-larger-than-buffer window)))

;;;###autoload
(defun +popup-alist-from-window-state (state)
  "Convert window STATE (from `window-state-get') to a `display-buffer' alist."
  (let* ((params (alist-get 'parameters state)))
    `((side          . ,(alist-get 'window-side params))
      (window-width  . ,(alist-get 'total-width state))
      (window-height . ,(alist-get 'total-height state))
      (window-parameters ,@params))))
