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


(defun +jg-misc-ivy-popup-buffer ()
  (interactive)
  (ivy-read "Popup Buffer: " #'internal-complete-buffer
            :keymap ivy-switch-buffer-map
            :action #'+jg-misc-ivy-open-as-popup
            :matcher #'ivy--switch-buffer-matcher
            :call 'jg-misc-ivy-popup-buffer
            )
  )

(defun +jg-misc-ivy-open-as-popup (buff)
  (let ((display-buffer-alist +popup--display-buffer-alist))
    (if (not (display-buffer-assq-regexp buff display-buffer-alist nil))
        (push (+popup-make-rule "." jg-misc-popup-persist-default)
              display-buffer-alist))
    (bury-buffer buff)
    (pop-to-buffer buff))
  )


(ivy-set-actions 'ivy-switch-buffer
                 '(("p" +jg-misc-ivy-open-as-popup "Popup")))

