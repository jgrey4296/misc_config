;;; editor/window-control/+funcs.el -*- lexical-binding: t; -*-

;;-- window ring
(defun +jg-ui-window-ring-block-reset (arg)
  (interactive "p")
  (window-ring-setup-columns arg t)
  )

;;-- end window ring

;;-- window rotation
;; From spacemacs originally
;; originally from magnars and modified by ffevotte for dedicated windows
;; support, it has quite diverged by now
(defun +jg-ui-window-rotate-forward (count)
  "Rotate each window forwards.
A negative prefix argument rotates each window backwards.
Dedicated (locked) windows are left untouched."
  (interactive "p")
  (let* ((non-dedicated-windows (cl-remove-if 'window-dedicated-p (window-list)))
         (states (mapcar #'window-state-get non-dedicated-windows))
         (num-windows (length non-dedicated-windows))
         (step (+ num-windows count)))
    (if (< num-windows 2)
        (error "You can't rotate a single window!")
      (dotimes (i num-windows)
        (window-state-put
         (elt states i)
         (elt non-dedicated-windows (% (+ step i) num-windows)))))))
;;-- end window rotation

;;-- layout toggle
;; from spacemacs originally:
;; from @bmag
(defun +jg-ui-window-layout-toggle ()
  "Toggle between horizontal and vertical layout of two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((window-tree (car (window-tree)))
             (current-split-vertical-p (car window-tree))
             (first-window (nth 2 window-tree))
             (second-window (nth 3 window-tree))
             (second-window-state (window-state-get second-window))
             (splitter (if current-split-vertical-p
                           #'split-window-horizontally
                         #'split-window-vertically)))
        (delete-other-windows first-window)
        ;; `window-state-put' also re-selects the window if needed, so we don't
        ;; need to call `select-window'
        (window-state-put second-window-state (funcall splitter)))
    (error "Can't toggle window layout when the number of windows isn't two.")))
;;-- end layout toggle

;;-- buffer opening
(defun +jg-ui-open-scratch-buffer (&optional arg)
  "Customised doom/open-project-scratch-buffer because it doesn't use pop-to-buffer "
  (interactive "P")
  (let (projectile-enable-caching)
    (funcall #'pop-to-buffer
     (doom-scratch-buffer
      arg
      (cond ((eq doom-scratch-initial-major-mode t)
             (unless (or buffer-read-only
                         (derived-mode-p 'special-mode)
                         (string-match-p "^ ?\\*" (buffer-name)))
               major-mode))
            ((null doom-scratch-initial-major-mode)
             nil)
            ((symbolp doom-scratch-initial-major-mode)
             doom-scratch-initial-major-mode))
      default-directory
        (doom-project-name)))))
;;-- end buffer opening

;;-- ui toggles
(defun +jg-ui-toggle-line-numbers ()
  (interactive)
  (setq-default display-line-numbers (if (not (eq display-line-numbers t)) t nil))
  )
(defun +jg-ui-toggle-line-numbers-visual ()
  (interactive)
  (setq-default display-line-numbers (if (not (eq display-line-numbers 'visual)) 'visual nil))
  )
(defun +jg-ui-toggle-window-dedication ()
  (interactive)
  (let ((curr-window (selected-window)))
    (set-window-dedicated-p curr-window
                            (not (window-dedicated-p curr-window)))
    (if (window-dedicated-p curr-window)
        (message "Window is now dedicated to %s" (window-buffer curr-window))
      (message "Window is not dedicated"))
    )
  )
(defun +jg-ui-toggle-line-move-ignore-invisible ()
  (interactive)
  (let ((newval (not line-move-ignore-invisible)))
    (setq-default line-move-ignore-invisible newval)
    (setq-local   line-move-ignore-invisible newval)
    (setq         line-move-ignore-invisible newval)
    )
  (message "Ignore invisible lines: %s" line-move-ignore-invisible)
  )
;;-- end ui toggles