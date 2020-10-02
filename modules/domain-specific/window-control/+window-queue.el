;;; domain-specific/window-control/+window-queue.el -*- lexical-binding: t; -*-

;; See: https://www.gnu.org/software/emacs/manual/html_node/elisp/Windows.html
;; Window Queue: Display N windows on screen, and have the be
;; views of adjacent buffers in a list
;; Be able to move to the most-recent, oldest, or along the list
;;

(setq-default +wc-ring-size 1
              +wc-ring-can-grow t
              +wc-ring-can-loop t
              +wc-ring-focus 0
              +wc-ring-windows '()
)

(map! :n "] \\" #'+wc-move-perspective
      :n "[ \\" #'+wc-move-perspective-2)


;; Add-to-list most-recent/oldest
(defun +wc-add-buffer-to-most-recent (buffer)
  (interactive "b")
  (ring-insert+extend +wc-ring buffer t)
  )
(defun +wc-add-buffer-to-oldest (buffer)
  (interactive "b")
  (ring-insert-at-beginning +wc-ring-size buffer)
  )

(defun +wc-clear-ring ()
  (setq +wc-ring (make-ring +wc-ring-size))
  (+wc-add-buffer-to-most-recent (buffer-name (current-buffer))))

(defun +wc-pop-buffer (arg)
  (interactive "p")
  (if (eq arg 4)
      ;;Pop everything from most-recent to here
      (let ((most-recent (ring-ref +wc-ring 0))
            (current (ring-ref +wc-ring +wc-ring-focus)))
            (while (and (ring-length +wc-ring)
                        (not (string-equal most-recent current)))
              (ring-remove +wc-ring (ring-member +wc-ring most-recent))
              (setq most-recent (ring-ref +wc-ring 0))
              )
            )
    ;; else pop the most recent
    (ring-remove +wc-ring 0)
    )
  )

(defun +wc-move-perspective (arg)
  (interactive "p")
  (let ((new-focus (if (eq arg 1)
                       (ring-minus1 +wc-ring-focus (length (cddr +wc-ring)))
                     (ring-plus1 +wc-ring-focus (length (cddr +wc-ring))))))
    ;; If can't loop, clamp and put empty buffers in windows
    ;; use get-buffer-create
    (message "Focus %s -> %s" +wc-ring-focus new-focus)
    (setq +wc-ring-focus new-focus)
    )
  ;; Redisplay
  (+wc-redisplay)
  )

(defun +wc-move-perspective-2 ()
  (interactive)
  (+wc-move-perspective 4))

(defun +wc-goto-most-recent ()
  (interactive)
  (setq +wc-ring-focus (ring-minus1
                        (ring-minus1 0 (length (cddr +wc-ring)))
                        (length (cddr +wc-ring))))
  (message "Focus: %s" +wc-ring-focus)
  )

(defun +wc-goto-oldest ()
  (interactive)
  (setq +wc-ring-focus 1)
  )

(defun +wc-redisplay ()
  (interactive)
  (let* ((centre (ring-ref +wc-ring +wc-ring-focus))
         (leftmost (ring-next +wc-ring centre))
         (rightmost (ring-previous +wc-ring centre)))
    ;;Assign to the column windows
    ;; If cant loop, pad leftmost and rightmost with empy buffers
    (message "Trio: %s -> %s -> %s" leftmost centre rightmost)
    (if (and (eq (length +wc-ring-windows) 3)
             (not (member nil (mapcar 'window-live-p +wc-ring-windows))))
        (mapc #'(lambda (xy) (set-window-buffer (cadr xy) (car xy)))
              (-zip-lists (list leftmost centre rightmost)
                          +wc-ring-windows))
      (setq +wc-ring-windows '()
            +wc-ring nil
            +wc-ring-focus 0)
      )))

(defun +wc-setup-window-columns (arg)
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
    (setq +wc-ring (make-ring +wc-ring-size)
          +wc-ring-windows (list leftmost centre rightmost))
    )
  (balance-windows)
  (+wc-add-buffer-to-head (buffer-name (current-buffer)))
  )

(defun +wc-print-order ()
  (interactive)
 (let ((elements (ring-elements +wc-ring)))
    ;; Print in a temporary buffer
    (with-temp-buffer-window "*Window Ring*"
        'display-buffer-pop-up-window
        nil
      (princ "Ring Buffers:\n\t")
      (princ (string-join elements "\n\t"))
      )
    )
  )
