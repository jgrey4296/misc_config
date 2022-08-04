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
;;-- end layout toggle

;;-- faces
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
    (cl-assert (<= num (length colors)))

    (loop for n to (- num 1) do
          (insert "(defface " name "-face-" (number-to-string n) "\n")
          (insert "  '((t :foreground \"" (nth n colors) "\"))\n")
          (insert "  \"Generated " name " " (number-to-string n) " Face\"\n)\n\n")
          )
    )
  )
;;-- end faces

;;-- narrowing
(defun +jg-ui-narrow-around-point ()
  (interactive)
  (cond (current-prefix-arg
         (narrow-to-region (line-beginning-position)
                           (point-max)))
        ((eq evil-state 'visual)
         (narrow-to-region evil-visual-beginning evil-visual-end))
        ((not (buffer-narrowed-p))
         (let ((num (read-number "Lines Around Point to Select: ")))
           (narrow-to-region (line-beginning-position (- num))
                             (line-end-position num))
           )
         )
        (t
         (widen))
        )
  )
(defun +jg-ui-toggle-narrow-buffer (arg)
  "Narrow the buffer to BEG END. If narrowed, widen it.
If region isn't active, narrow away anything above point
"
  (interactive "P")
  (cond ((eq evil-state 'normal)
         (narrow-to-region (line-beginning-position) (point-max)))
        ((eq evil-state 'visual)
         (narrow-to-region evil-visual-beginning evil-visual-end))
        )
  )
(defun +jg-ui-narrowing-move-focus-backward (arg)
  (interactive "p")
  (+jg-ui-narrowing-move-focus-forward(- arg))
  )
(defun +jg-ui-narrowing-move-focus-forward (arg)
  (interactive "p")
  (widen)
  (evil-forward-section-begin arg)
  (let ((bounds (+evil:defun-txtobj)))
    (narrow-to-region (car bounds) (cadr bounds))
    )
  )
;;-- end narrowing

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
  (setq display-line-numbers (if (not (eq display-line-numbers t)) t nil))
  )
(defun +jg-ui-toggle-line-numbers-visual ()
  (interactive)
  (setq display-line-numbers (if (not (eq display-line-numbers 'visual)) 'visual nil))
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
  (setq line-move-ignore-invisible (not line-move-ignore-invisible))
  (message "Ignore invisible lines: %s" line-move-ignore-invisible)
  )
;;-- end ui toggles

;;-- popup control
(defun +jg-ui-popup-add-rules (sym rules &optional override)
  " sym is a symbol to avoid adding duplicate rulesets

  Expects a list of form:
  '((PATTERN :opt val :opt val) (PATTERN :opt val :opt val))
  "
  (cl-assert (hash-table-p jg-popup-display-rules))
  (if (and (gethash sym jg-popup-display-rules) (not override))
      (message "Popup Ruleset %s already exists" sym)
    (puthash sym (cl-loop for (head . body) in rules
                          for priority = (* -1 (or (plist-get body :priority) 0))
                          collect (cons priority (+popup-make-rule head body)))
             jg-popup-display-rules)
    )
  )

(defun +jg-ui-popup-activate-rules ()
  (message "Activating Popup rules: %s" (hash-table-keys jg-popup-display-rules))
  (let ((all-rules (copy-sequence (-flatten-n 1 (hash-table-values jg-popup-display-rules)))))
    (setq +popup--display-buffer-alist nil
          display-buffer-alist (mapcar #'cdr
                                       (sort all-rules #'(lambda (x y)
                                                           (< (car x) (car y))))))
    )
  )

(define-advice set-popup-rules! (:after (&rest args)
                                 +jg-popup-advice)
  (+jg-ui-popup-activate-rules))

(define-advice set-popup-rule! (:after (&rest args)
                                 +jg-popup-advice2)
  (+jg-ui-popup-activate-rules))
;;-- end popup control
