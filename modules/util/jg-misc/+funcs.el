;;; util/jg-misc/+funcs.el -*- lexical-binding: t; -*-

(defun +jg-misc-undo-tree ()
  (interactive)
  (if (not undo-tree-mode)
      (undo-tree-mode))
   (undo-tree-visualize)

  )

;; From spacemacs originally
;; originally from magnars and modified by ffevotte for dedicated windows
;; support, it has quite diverged by now
(defun +jg-rotate-windows-forward (count)
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


;; from spacemacs originally:
;; from @bmag
(defun +jg-window-layout-toggle ()
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


(defun +jg-misc-open-scratch-buffer (&optional arg)
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

(defun +jg-misc-yank-buffer-name ()
  (interactive)
  (message (kill-new (buffer-name)))
  )

(defun +jg-misc-ivy-predicate (x)
  ;; return nil for cruft buffers
  x
  )

(defun +jg-misc-ivy-switch-buffer ()
  (interactive)
  (ivy-read "Switch to buffer: " #'internal-complete-buffer
            :keymap ivy-switch-buffer-map
            :predicate #'+jg-misc-ivy-predicate
            :preselect (buffer-name (other-buffer (current-buffer)))
            :action #'ivy--switch-buffer-action
            :matcher #'ivy--switch-buffer-matcher
            :caller 'ivy-switch-buffer)
  )
