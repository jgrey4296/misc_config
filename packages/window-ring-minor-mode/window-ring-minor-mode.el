;;; ../../../Volumes/documents/github/emacs_files/packages/window-ring-minor-mode/window-ring-minor-mode.el -*- lexical-binding: t; -*-

;;; domain-specific/window-control/window-queue.el -*- lexical-binding: t; -*-

;; See: https://www.gnu.org/software/emacs/manual/html_node/elisp/Windows.html
;; Window Queue: Display N windows on screen, and have the be
;; views of adjacent buffers in a list
;; Be able to move to the most-recent, oldest, or along the list
;;




;; Add-to-list most-recent/oldest
(defun window-ring-add-current-buffer ()
  (interactive)
  (window-ring-add-to-head (buffer-name (current-buffer)))
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
        (face-remap-set-base 'default :background (nth window-ring-background-index window-ring-background-color-options))
        )
      (cl-incf window-ring-background-index)
      (if (>= window-ring-background-index (length window-ring-background-color-options))
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
  (setq window-ring (make-ring window-ring-size))
  (window-ring-add-to-head (buffer-name (current-buffer))))
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
  )

(defun window-ring-remove-buffer (&optional buff-name)
  (interactive)
  (let* ((buff (if buff-name buff-name (buffer-name (current-buffer))))
         (mem (if window-ring (ring-member window-ring buff) nil)))
    (if mem
        (progn (message "Removing: %s" mem)
               (ring-remove window-ring mem)))))


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
  (let* ((centre (or (ring-ref window-ring window-ring-focus) window-ring-nil-buffer-name))
         (leftmost (if (or window-ring-can-loop (< window-ring-focus (- (ring-length window-ring) 1)))
                            (ring-next window-ring centre)
                   window-ring-nil-buffer-name))
         (rightmost (if (or window-ring-can-loop (> window-ring-focus 0))
                        (ring-previous window-ring centre)
                      window-ring-nil-buffer-name)
                    )
         )
    ;;Assign to the column windows
    ;; If cant loop, pad leftmost and rightmost with empy buffers
    (message "Trio: %s -> %s -> %s" leftmost centre rightmost)
    (if (and (eq (length window-ring-windows) 3)
             (not (member nil (mapcar 'window-live-p window-ring-windows))))
        (mapc #'(lambda (xy) (set-window-buffer (cadr xy) (car xy)))
              (-zip-lists (list leftmost centre rightmost)
                          window-ring-windows))
      (setq window-ring-windows '()
            window-ring nil
            window-ring-focus 0)
      )))
(defun window-ring-setup-columns (arg)
  (interactive "p")
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
    (setq window-ring (make-ring window-ring-size)
          window-ring-windows (list leftmost centre rightmost))
    )
  (balance-windows)
  (window-ring-add-to-head (buffer-name (current-buffer)))
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
                window-ring-background-color-options '("gray11" "gray13" "gray15" "gray17" "gray19" "gray21" "gray23" "gray25")
              )
  (get-buffer-create window-ring-nil-buffer-name)
  (add-hook 'kill-buffer-hook #'window-ring-remove-buffer)
  )

(provide 'window-ring-minor-mode)
