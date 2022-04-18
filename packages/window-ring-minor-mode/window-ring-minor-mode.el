;;; ../../../Volumes/documents/github/emacs_files/packages/window-ring-minor-mode/window-ring-minor-mode.el -*- lexical-binding: t; -*-

;;; domain-specific/window-control/window-queue.el -*- lexical-binding: t; -*-

;; See: https://www.gnu.org/software/emacs/manual/html_node/elisp/Windows.html
;; Window Queue: Display N windows on screen, and have the be
;; views of adjacent buffers in a list
;; Be able to move to the most-recent, oldest, or along the list
;;

;; To expand to vertical:
;; Add to ring means add a ring to the ring, and add buffer there

;; Ring_prime [ Sub-Ring1[a b ] Sub-Ring2[c d ] Sub-Ring3[e f ]]

;; Control: Add to new sub-ring, or add to top of current sub-ring
;; Display: 3 columns, middle column divided in 3

;; Add-to-list most-recent/oldest
(defun window-ring-add-current-buffer ()
  (interactive)
  (window-ring-add-to-head (buffer-name (current-buffer)))
  (window-ring-redisplay)
  )
(defun window-ring-add-to-head (&optional buffer)
  (interactive)
  (if buffer
      (progn (message "Adding %s to %s" buffer (ring-elements window-ring))
             (ring-insert+extend window-ring buffer t))
    (let* ((display-buffer-alist '(("^" display-buffer-no-window)))
           (file (counsel--find-file-1 "Add File to Ring: " nil nil 'window-ring-add-to-head))
           (found-buffer (find-file file))
           (buff-name (buffer-name found-buffer))
           )

      (message "File-name: %s : %s" buff-name file)
      (cond ((not (ring-member window-ring buff-name))
             (ring-insert+extend window-ring buff-name window-ring-can-grow))
            (window-ring-allow-duplicates
             (ring-insert+extend window-ring (buffer-name (make-indirect-buffer buff-name (generate-new-buffer-name buff-name) t))
                                 window-ring-can-grow))
            (t
             (message "Buffer %s already in window ring: %s" buff-name (ring-elements window-ring)))
            )
      ;; Set buffer-local background
      (with-current-buffer buff-name
        (message "REMAPPING: %s : %s" window-ring-background-index (nth window-ring-background-index window-ring-background-color-options))
        (face-remap-set-base 'default :background (nth window-ring-background-index window-ring-background-color-options))
        )
      (cl-incf window-ring-background-index)
      (if (<= (length window-ring-background-color-options) window-ring-background-index)
          (setq window-ring-background-index 0))

      )
    )
  )
(defun window-ring-add-to-tail (buffer)
  (interactive "b")
  (message "Adding %s to %s" buffer (ring-elements window-ring))
  (ring-insert-at-beginning window-ring (get-buffer buffer))
  )

(defun window-ring-clear-ring ()
  (interactive)
  (message "Clearing Window Ring")
  (setq window-ring (make-ring window-ring-size)
        window-ring-background-index 0
        )
  (window-ring-add-to-head (buffer-name (current-buffer)))
  (window-ring-redisplay)
)
(defun window-ring-pop-buffer (arg)
  (interactive "p")
  (if (eq arg 4)
      ;;Pop everything from most-recent to here
      (let ((most-recent #'(lambda () (ring-ref window-ring 0)))
            (current (ring-ref window-ring window-ring-focus)))
            (while (and (ring-length window-ring)
                        (not (string-equal (funcall most-recent)
                                           current)))
              (ring-remove window-ring 0)
              )
            )
    ;; else pop the most recent
    (ring-remove window-ring 0)
    )
  (window-ring-redisplay)
  )

(defun window-ring-remove-buffer (&optional buff-name)
  " Remove the current buffer from the ring "
  (interactive)
  (if (and window-ring-minor-mode window-ring)
      (let* ((buff (if buff-name buff-name (buffer-name (current-buffer))))
             (mem (if window-ring (ring-member window-ring buff) nil)))
        (if mem
            (progn (message "Removing: %s" mem)
                   (ring-remove window-ring mem)
                   (if (not buff-name)
                       (window-ring-redisplay)))))
    )
)
(defun window-ring-replace-buffer(&optional buff-name)
  " Replace the current central window's buffer with the current buffer "
  (interactive)
 (if (and window-ring-minor-mode window-ring)
      (let* ((buff (if buff-name buff-name (buffer-name (current-buffer))))
             (mem (if window-ring (ring-member window-ring buff) nil))
             (centre (ring-ref window-ring window-ring-focus))
             (ring-list (ring-elements window-ring)))
        (if (not mem)
            (progn
              (setf (nth (position centre ring-list :test 's-equals?) ring-list) buff)
              (setq window-ring (ring-convert-sequence-to-ring ring-list)))
          )
        )
   )
 )

(defun window-ring-move-perspective (arg)
  (interactive "p")
  (let ((new-focus (cond ((and window-ring-can-loop (eq arg 1))
                          (ring-minus1 window-ring-focus (length (cddr window-ring))))
                         (window-ring-can-loop
                          (ring-plus1 window-ring-focus (length (cddr window-ring))))
                         ((eq arg 1)
                          (max 0 (- window-ring-focus 1)))
                         (t (min (- (ring-length window-ring) 1) (+ window-ring-focus 1))))))
    ;; If can't loop, clamp and put empty buffers in windows
    ;; use get-buffer-create
    (message "Focus %s -> %s" window-ring-focus new-focus)
    (setq window-ring-focus new-focus)
    )
  ;; Redisplay
  (window-ring-redisplay)
  )
(defun window-ring-move-perspective-2 ()
  (interactive)
  (window-ring-move-perspective 4))

(defun window-ring-goto-most-recent ()
  (interactive)
  (setq window-ring-focus 0)
  (message "Focus: %s" window-ring-focus)
  (window-ring-redisplay)
  )
(defun window-ring-goto-oldest ()
  (interactive)
  (setq window-ring-focus (- (ring-length window-ring) 1))
  (window-ring-redisplay)
  )

(defun window-ring-redisplay ()
  (interactive)
  (if (eq (length window-ring-windows)
          (length (-remove 'not (mapcar 'window-valid-p window-ring-windows))))
      (window-ring-redisplay-unguarded)
      ))

(defun window-ring-redisplay-unguarded ()
  (let* ((at-start (eq 0 window-ring-focus))
         (at-end (eq window-ring-focus (- (ring-length window-ring) 1)))
         (len-one (eq 1 (ring-length window-ring)))
         (centre (ring-ref window-ring window-ring-focus))
         (leftmost (if (or len-one (and at-end (not window-ring-can-loop)))
                       window-ring-nil-buffer-name
                     (ring-next window-ring centre)))
         (rightmost (if (or len-one (and at-start (not window-ring-can-loop)))
                        window-ring-nil-buffer-name
                      (ring-previous window-ring centre)))
         )
    ;; Assign to the column windows
    ;; If cant loop, pad leftmost and rightmost with empy buffers
    (message "Trio: %s -> %s -> %s" leftmost centre rightmost)
    (if (and (eq (length window-ring-windows) 3)
             (not (member nil (mapcar 'window-live-p window-ring-windows))))
        (mapc #'(lambda (xy) (set-window-buffer (cadr xy) (car xy)))
              (-zip-lists (list leftmost centre rightmost)
                          window-ring-windows)))
    (select-window (nth 1 window-ring-windows))
      ))

(defun window-ring-setup-columns-command (arg)
  (interactive "p")
  (window-ring-setup-columns arg)
  )

(defun window-ring-setup-columns (arg &optional soft)
  " Reset windows to (or arg 3) columns.
    if SOFT then don't clear the window ring
  "
  ;; (arg == 1 -> one row) (else -> two rows, only use top)
  ;; Clear
  (if (not window-ring-minor-mode)
      (window-ring-minor-mode 1))

  (delete-other-windows)
  (let ((leftmost (selected-window))
        centre rightmost)
  ;; split
   (message "ARG: %s" arg)
    (if (eq arg 4)
        ;; Split as top 3 columns
        (split-window-below)
      )

    ;; Split as 3 columns
    (setq centre (split-window-right))
    (select-window centre)
    (setq rightmost (split-window-right))

    ;; init ring

    (if (not (and soft window-ring))
        (progn
          (setq window-ring (make-ring window-ring-size))
          (window-ring-add-to-head (buffer-name (current-buffer))))
      (message "Soft window-ring reset")
      )

    (setq window-ring-windows (list leftmost centre rightmost))
    )
  (balance-windows)
  (window-ring-redisplay)
  )

(defun window-ring-print-order ()
  (interactive)
  (let* ((curr-elem (ring-ref window-ring window-ring-focus))
         (elements (ring-elements window-ring))
         (marked-elements (mapcar #'(lambda (x)
                                      (if (string-equal x curr-elem)
                                          (format "%s  <--- Focus" x)
                                        x)) elements))
         )
    ;; Print in a temporary buffer
    (with-temp-buffer "*Window Ring*"
        ;; '(display-buffer-in-side-window . ((side . right) (slot . 1)))
        ;; '(lambda (win val) (delete-window win))
      (princ "Ring Buffers:\n\t")
      (princ (string-join marked-elements "\n\t"))
      )
    )
  )

(defun window-ring-shrink-sides (amt)
  (interactive "NShrink By: ")
  (with-selected-window (car (last window-ring-windows))
    (shrink-window-horizontally amt)
      )
  (with-selected-window (first window-ring-windows)
    (shrink-window-horizontally amt)
    )
  )

(defun window-ring-edit-order ()
  (interactive)
  (let ((new-window (split-window (selected-window) nil 'below))
        (elements (ring-elements window-ring)))
    (set-window-buffer new-window (get-buffer-create "*window-ring-edit*"))
    (select-window new-window)
    (message "Elements: %s" elements)
    (with-current-buffer "*window-ring-edit*"
      (auto-save-mode -1)
      (set-window-text-height (selected-window) 10)
      (set (make-local-variable 'backup-inhibited) t)
      (window-ring-edit-minor-mode)
      (erase-buffer)
      (mapc (lambda (x) (insert (format "%s\n" x))) elements)
      )
    (redraw-display)
    )
  )

(define-minor-mode window-ring-minor-mode
  "A Minor Mode for easy control of a 3-ple view of a ring of buffers"
  :lighter "Window-Ring"
  :global t
  (setq-default window-ring-size 1
                window-ring-can-grow t
                window-ring-can-loop t
                window-ring-allow-duplicates t
                window-ring-focus 0
                window-ring-windows '()
                window-ring nil
                window-ring-nil-buffer-name "*Window-Ring Buffer*"
                window-ring-background-index 0
                window-ring-background-color-options '("gray19" "lemonchiffon4")
              )
  (get-buffer-create window-ring-nil-buffer-name)
  (add-hook 'kill-buffer-hook #'window-ring-remove-buffer)
  )

(defun window-ring-edit-commit ()
  (interactive)
  (let ((order (s-split "\n" (buffer-substring-no-properties (point-min) (point-max)) t)))
    (setq window-ring (make-ring window-ring-size)
          window-ring-background-index 0
          )
    (mapc #'window-ring-add-to-head order)
    (kill-buffer-and-window)
    )
  )

(setq window-ring-edit-map (make-sparse-keymap))
(evil-define-key '(normal insert) window-ring-edit-map (kbd "C-c C-c") #'window-ring-edit-commit)

(map! :map window-ring-edit-map
      "C-c C-c" #'window-ring-edit-commit)


(define-minor-mode window-ring-edit-minor-mode
  " A Minor mode to commit changes to the order of window ring buffers "
  :lighter "Window-Ring-Edit"
  :keymap window-ring-edit-map
  )

(provide 'window-ring-minor-mode)
