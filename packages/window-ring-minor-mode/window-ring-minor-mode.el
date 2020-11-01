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
      (ring-insert+extend window-ring buffer t)
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
  (ring-insert-at-beginning window-ring buffer)
  )

(defun window-ring-clear-ring ()
  (interactive)
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
  (window-ring-minor-mode 1)
  (window-ring-setup-columns arg)
  )

(defun window-ring-setup-columns (arg &optional soft)
  ;; (arg == 1 -> one row) (else -> two rows, only use top)
  ;; Clear
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
          (window-ring-add-to-head (buffer-name (current-buffer)))))

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

(provide 'window-ring-minor-mode)
