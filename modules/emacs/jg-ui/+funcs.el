;;; editor/window-control/+funcs.el -*- lexical-binding: t; -*-

(defun +jg-ui-window-ring-block-reset (arg)
  (interactive "p")
  (window-ring-setup-columns arg t)
  )

(defun +jg-ui-undo-tree ()
  (interactive)
  (if (not undo-tree-mode)
      (undo-tree-mode))
  (undo-tree-visualize)

  )

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
(after! core-ui
  (message "After core-ui")
  (advice-remove 'kill-current-buffer #'doom--switch-to-fallback-buffer-maybe-a)
  ;; Originally from doom/core/core-ui
  (defadvice! +jg-ui-kill-buffer-override (&rest _)
    "Switch to `doom-fallback-buffer' if on last real buffer.

Advice for `kill-current-buffer'. If in a dedicated window, delete it. If there
are no real buffers left OR if all remaining buffers are visible in other
windows, switch to `doom-fallback-buffer'. Otherwise, delegate to original
`kill-current-buffer'."
    :before-until #'kill-current-buffer
    (let ((buf (current-buffer)))
      (cond ((window-dedicated-p)
             (delete-window)
             t)
            ((eq buf (doom-fallback-buffer))
             (message "Can't kill the fallback buffer.")
             t)
            ((doom-real-buffer-p buf)
             (let ((visible-p (delq (selected-window) (get-buffer-window-list buf nil t))))
               (unless visible-p
                 (when (and (buffer-modified-p buf)
                            (not (y-or-n-p
                                  (format "Buffer %s is modified; kill anyway?"
                                          buf))))
                   (user-error "Aborted")))
               (let ((inhibit-redisplay t)
                     (doom-inhibit-switch-buffer-hooks t)
                     buffer-list-update-hook)
                 (when (or ;; if there aren't more real buffers than visible buffers,
                        ;; then there are no real, non-visible buffers left.
                        (not (cl-set-difference (doom-real-buffer-list)
                                                (doom-visible-buffers)))
                        ;; if we end up back where we start (or previous-buffer
                        ;; returns nil), we have nowhere left to go
                        (memq (switch-to-prev-buffer nil t) (list buf 'nil)))
                   (switch-to-buffer (doom-fallback-buffer)))
                 (unless visible-p
                   (with-current-buffer buf
                     (restore-buffer-modified-p nil))
                   (kill-buffer buf)))
               (run-hooks 'doom-switch-buffer-hook 'buffer-list-update-hook)
               t)))))
  )


(defun +jg-ui-insert-faces ()
  "insert lisp code for a set of faces automatically"
  (interactive)
  (let ((name (read-string "Face Names: "))
        (num (read-number "Number of Faces: "))
        (file (read-file-name "File: " jg-ui-default-face-gen-palette-dir))
        colors
        )

    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "#[[:alnum:]]+" nil t)
        (push (match-string 0) colors)
        ))
    (assert (<= num (length colors)))

    (loop for n to (- num 1) do
          (insert "(defface " name "-face-" (number-to-string n) "\n")
          (insert "  '((t :foreground \"" (nth n colors) "\"))\n")
          (insert "  \"Generated " name " " (number-to-string n) " Face\"\n)\n\n")
          )
    )
  )
