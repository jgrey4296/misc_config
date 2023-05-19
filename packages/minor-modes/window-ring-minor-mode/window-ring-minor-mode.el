;;; window-ring-minor-mode/window-ring-minor-mode.el -*- lexical-binding: t; -*-
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
(require 'evil)
(require 'persp-mode)
(require 'cl-lib)

(defvar window-ring--adding nil)

(defvar window-ring-suppress-adding nil)

(defvar window-ring-buffer-test-fn 'identity "one argument, current buffer, return non-nil to add to current ring")

(defvar window-ring-column-fn #'window-ring-setup-columns-default)

(defmacro with-window-ring (&rest body)
  (declare (indent 1))
  `(when (persp-parameter 'window-ring)
     (let ((wr-persp      (get-current-persp))
           (wr-actual     (persp-parameter 'window-ring-actual))
           (wr-grow       (persp-parameter 'window-ring-grow))
           (wr-loop       (persp-parameter 'window-ring-loop))
           (wr-duplicates (persp-parameter 'window-ring-duplicates))
           (wr-focus      (persp-parameter 'window-ring-focus))
           (wr-max        (persp-parameter 'window-ring-max))
           (wr-scratch    (persp-parameter 'window-ring-scratch))
           )
       ,@body
       )
     )
  )

(defmacro with-other-window-ring (persp &rest body)
  (declare (indent 1))
  `(when (persp-parameter 'window-ring persp)
     (let ((wr-persp      persp)
           (wr-actual     (persp-parameter 'window-ring-actual persp))
           (wr-grow       (persp-parameter 'window-ring-grow persp))
           (wr-loop       (persp-parameter 'window-ring-loop persp))
           (wr-duplicates (persp-parameter 'window-ring-duplicates persp))
           (wr-focus      (persp-parameter 'window-ring-focus persp))
           (wr-max        (persp-parameter 'window-ring-max persp))
           )
       ,@body
       )
     )
  )

(defmacro when-window-ring (&rest body)
  (declare (indent 1))
  `(when (persp-parameter 'window-ring)
     ,@body
     )
  )

(defmacro with-window-ring-adding (&rest body)
  `(let ((window-ring--adding t))
     ,@body
     )
  )

(define-minor-mode window-ring-minor-mode
  "A Minor Mode for easy control of a 3-ple view of a ring of buffers"
  :lighter "Window-Ring"
  :global t
  (add-to-list 'persp-created-functions #'window-ring-create-persp-fn)
  (add-to-list 'persp-activated-functions #'window-ring-activate-persp-fn)
  (add-to-list 'persp-before-deactivate-functions #'window-ring-deactivate-persp-fn)
  (add-to-list 'persp-before-kill-functions #'window-ring-kill-persp-fn)
  (add-hook 'find-file-hook #'window-ring-add-current-buffer)
  (add-hook 'kill-buffer-hook #'window-ring-remove-buffer)
  (add-hook 'kill-buffer-query-functions #'window-ring-protect-scratch-p -50)
  )

(defun window-ring-p (&optional arg)
  (interactive "p")
  (when (persp-parameter 'window-ring)
    t)
  )

(defun window-ring-buffer-p (&optional arg buffer)
  (interactive "p")
  (when (and (window-ring-p)
             (ring-member (persp-parameter 'window-ring-actual)
                          (or buffer (current-buffer))))
    t)
  )

(defun window-ring-protect-scratch-p ()
  (not (with-window-ring
           (eq (current-buffer) wr-scratch)
         )
       )
  )

;;-- creation

(defun window-ring-new ()
  " create a new perspective and ring "
  (interactive)
  (message "Creating new window ring")
  (let ((ring-name (format "%s-WR" (read-string "New Ring: ")))
        (curr (current-buffer))
        )
    (with-window-ring-adding
     (persp-add-new ring-name)
     (persp-switch ring-name)
     (add-hook 'find-file-hook              #'window-ring-add-current-buffer)
     (add-hook 'kill-buffer-hook            #'window-ring-remove-buffer)
     (add-hook 'kill-buffer-query-functions #'window-ring-protect-scratch-p -50)
     (switch-to-buffer (persp-parameter 'window-ring-scratch))
     (window-ring-add-to-head curr)
     (window-ring-reset-columns t)
     )
    )
  )

(defun window-ring-create-persp-fn (persp hash)
  (message "Initializing window ring %s" window-ring--adding)
  (when window-ring--adding
    (modify-persp-parameters `((window-ring . t)
                               (window-ring-actual . ,(make-ring 1))
                               (window-ring-grow . t)
                               (window-ring-loop . t)
                               (window-ring-duplicates . t)
                               (window-ring-focus . 0)
                               (window-ring-max . -1)
                               (window-ring-backgrounds . ("gray19" "gray12" "gray4"))
                               (window-ring-scratch . ,(get-buffer-create (format "*%s*" (safe-persp-name persp))))
                               )
                             persp
                             )
    (ring-insert+extend (persp-parameter 'window-ring-actual persp)
                        (persp-parameter 'window-ring-scratch persp))
    )
  )

(defun window-ring-activate-persp-fn (type)
  (when (persp-parameter 'window-ring)
    (cond ('frame)
          ('window

           )
          )
    )
  )

(defun window-ring-deactivate-persp-fn (type)
  (when (persp-parameter 'window-ring)
    (cond ('frame)
          ('window)
          )
    )
  )

(defun window-ring-reset-columns (&optional arg)
  (interactive "p")
  (window-ring-setup-columns arg t)
  )

(defun window-ring-setup-columns (&optional arg soft)
  " Reset windows using `window-ring-column-fn`
    if SOFT then don't clear the window ring "
  (interactive "pi")
  (persp-delete-other-windows)
  (funcall window-ring-column-fn arg soft)

  ;; init ring
  (unless soft
    (modify-persp-parameters `((window-ring-actual . ,(make-ring 1))))
    (window-ring-add-current-buffer)
    )

  (when arg
    (window-ring-redisplay))
  )

(defun window-ring-setup-columns-default (&optional arg soft)
  ;; (arg == 1 -> one row) (else -> two rows, only use top)
  ;; Clear
  (let ((leftmost (selected-window))
        centre rightmost)
    ;; split
    (when (and (numberp arg) (< 1 arg)) ;; Split as top 3 columns
      (split-window-below))

    ;; Split as 3 columns
    (setq centre (split-window-right))
    (select-window centre)
    (setq rightmost (split-window-right))

    (when (and arg evil-auto-balance-windows)
      (balance-windows))
    )
  )

(defun window-ring-setup-columns-alt (&optional arg soft)
  " "
  (let ((leftmost (selected-window))
        )
    (split-window-right)
    (split-window-right)

    )
  )

(defun window-ring-setup-vertical (&optional arg soft)
  (split-window-below)
  (split-window-below)

  (when (and arg evil-auto-balance-windows)
    (balance-windows))
  )

(defun window-ring-kill-persp-fn (persp)
  (with-other-window-ring persp
      (message "Killing Window Ring")
    )
  )

;;-- end creation

;;-- addition

(defun window-ring-add-current-buffer (&optional arg)
  (interactive "p")
  (when (and (persp-parameter 'window-ring)
             (or (buffer-local-boundp 'window-ring-buffer (current-buffer))
                 (funcall window-ring-buffer-test-fn (current-buffer)))
             (not window-ring-suppress-adding)
             )
    (window-ring-add-to-head (current-buffer) arg)
    )
  )

(defun window-ring-add-to-head (buffer &optional arg)
  (interactive "b\np")
  (with-window-ring
      (-when-let (buff (get-buffer buffer))
        (message "Adding buffer to window ring: %s" buffer)
        (ring-insert+extend wr-actual buff t)
        )
    )
  (when arg (window-ring-redisplay))
  )

(defun window-ring-add-to-tail (buffer &optional arg)
  (interactive "b\np")
  (with-window-ring
      (-when-let (buff (get-buffer buffer))
        (ring-insert-at-beginning window-ring buffer))
    )
  (when arg (window-ring-redisplay))
  )

;;-- end addition

;;-- removal

(defun window-ring-clear-ring (&optional arg)
  (interactive "p")
  (with-window-ring
      (message "Clearing Window Ring")
    (modify-persp-parameters `((window-ring-actual . ,(make-ring 1))
                               (window-ring-focus . 0)
                               ) wr-persp)
    )
  (window-ring-add-current-buffer arg)
  )

(defun window-ring-pop-buffers (&optional arg)
  (interactive "p")
  (with-window-ring
      ;;Pop everything from most-recent to here
      (let ((most-recent (-partial #'ring-ref wr-actual 0))
            (target (ring-ref wr-actual wr-focus))
            )
        (while (and (ring-length wr-actual)
                    (not (eq (funcall most-recent) target)))
          (ring-remove wr-actual 0))
        )
    (ring-resize wr-actual (ring-length wr-actual))
    )
  (when arg (window-ring-redisplay))
  )

(defun window-ring-remove-buffer (&optional buffer arg)
  " Remove the current buffer from the ring "
  (interactive "b\np")
  (with-window-ring
      (let* ((buff (if (bufferp buffer) buffer (current-buffer)))
             (index (unless (ring-empty-p wr-actual)
                      (ring-member wr-actual buff)))
             )
        (when index
          (ring-remove wr-actual index)
          (ring-resize wr-actual (ring-length wr-actual))
          )
        )
    )
  (when arg (window-ring-redisplay))
  )

(defun window-ring-replace-buffer(&optional buffer arg)
  " Replace the current focus buffer with the current buffer "
  (interactive "b\np")
  ;; TODO use with-other-window-ring
  (with-window-ring
      (let* ((buff (if (bufferp buffer) buffer (current-buffer)))
             (empty-p (ring-empty-p wr-actual))
             (index (unless empty-p (ring-member wr-actual buff)))
             (centre (unless empty-p (ring-ref wr-actual wr-focus)))
             (ring-list (unless empty-p (ring-elements wr-actual)))
             )
        (when index
          (setf (nth (cl-position centre ring-list) ring-list) buff)
          (modify-persp-parameters `((window-ring-actual .
                                      ,(ring-convert-sequence-to-ring ring-list)))
                                   )
          )
        )
    )
  (when arg (window-ring-redisplay))
  )

;;-- end removal

;;-- movement

(defun window-ring-toggle-loop ()
  (interactive)
  (with-window-ring
      (modify-persp-parameters `((window-ring-loop . ,(not wr-loop))))
    )
  )

;;;###autoload
(defun window-ring-move-focus (&optional arg)
  " move the focus towards the most recent addition to window ring.
 if arg is not nil, move towards oldest "
  (interactive "p")
  (cond ((persp-parameter 'window-ring)
         (let* ((wr-persp      (get-current-persp))
                (wr-actual     (persp-parameter 'window-ring-actual))
                (wr-grow       (persp-parameter 'window-ring-grow))
                (wr-loop       (persp-parameter 'window-ring-loop))
                (wr-duplicates (persp-parameter 'window-ring-duplicates))
                (wr-focus      (persp-parameter 'window-ring-focus))
                (wr-max        (persp-parameter 'window-ring-max))
                (wr-scratch    (persp-parameter 'window-ring-scratch))
                (new-focus (if (< 1 arg)
                               (window-ring-older (ring-length wr-actual) wr-focus wr-loop)
                             (window-ring-newer (ring-length wr-actual) wr-focus wr-loop)))
                )
           (when new-focus
             (modify-persp-parameters `((window-ring-focus . ,new-focus)))
             )
           )
         (window-ring-redisplay)
         )
        ((< 1 arg)
         (evil-window-left 1))
        (t
         (evil-window-right 1))
        )
  )

;;;###autoload
(defun window-ring-move-focus-alt ()
  (interactive)
  (window-ring-move-focus 2)
  )

(defun window-ring-goto-most-recent (&optional arg)
  (interactive "p")
  (with-window-ring
      (modify-persp-parameters '((window-ring-focus . 0)))
    )
  (when arg (window-ring-redisplay))
  )

(defun window-ring-goto-oldest (&optional arg)
  (interactive)
  (with-window-ring
      (modify-persp-parameters '((window-ring-focus . -1)))
    )
  (when arg (window-ring-redisplay))
  )

;;-- end movement

;;-- display

(defun window-ring-redisplay ()
  (interactive)
  (when-window-ring
      (window-ring-redisplay-actual)
    )
  )

(defun window-ring-redisplay-actual ()
  (with-window-ring
      (let* ((largest  (get-largest-window 'visible))
             (ring-len (ring-length wr-actual))
             (curr-win (window-next-sibling largest))
             (index    (window-ring-newer ring-len wr-focus wr-loop))
             )
        (message "Windows: Largest: %s (%s) curr: %s (%s)" largest (window-live-p largest) curr-win (window-live-p curr-win))
        (message "Window-ring-state: (%s) %s" ring-len wr-actual)
        (set-window-buffer largest (ring-ref wr-actual wr-focus))
        (while curr-win
          (window-ring-set-window curr-win index)
          (setq curr-win (window-next-sibling curr-win)
                index (window-ring-newer ring-len index wr-loop)))
        (setq index (window-ring-older ring-len wr-focus wr-loop)
              curr-win (window-prev-sibling largest))
        (while curr-win
          (window-ring-set-window curr-win index)
          (setq curr-win (window-prev-sibling curr-win)
                index (window-ring-older ring-len index wr-loop))
          )
        )
    )
  )

(defun window-ring-newer (len index loop)
  (pcase index
    ((pred null) nil)
    ((or (guard loop) (guard (< 0 index)))
     (ring-minus1 index len))
    (_ nil)
    )
  )

(defun window-ring-older (len index loop)
  (pcase index
    ((pred null) nil)
    ((or (guard loop) (guard (< index (1- len))))
     (ring-plus1 index len))
    (_ nil)
    )
  )

(defun window-ring-set-window (window index)
  (message "Window Ring Setting Window: %s (%s) : %s" window (window-live-p window) index)
  (with-window-ring
      (unless (window-live-p window) (select-window window))
    (set-window-buffer window (if index
                                  (ring-ref wr-actual index)
                                wr-scratch))
    )
  )

(defun window-ring-print-order ()
  (interactive)
  (with-window-ring
      (let ((index (window-ring-older (ring-length wr-actual) wr-focus nil))
            (len (ring-length wr-actual))
            (older ())
            (newer ())
            (focus (ring-ref wr-actual wr-focus))
            )
        (while index
          (push (buffer-name (ring-ref wr-actual index)) older)
          (setq index (window-ring-older len index nil)))
        (setq index (window-ring-newer len wr-focus nil))
        (while index
          (push (buffer-name (ring-ref wr-actual index)) newer)
          (setq index (window-ring-newer len index nil)))
        (with-temp-buffer (format "*WR Buffers: %s*" (persp-name (get-current-persp)))
                          (princ "Ring Buffers:\n")
                          (mapcar #'(lambda (x) (princ x) (princ " | ")) older)
                          (princ (format "[%s] | " (buffer-name focus)))
                          (mapcar #'(lambda (x) (princ x) (princ " | ")) (reverse newer))
                          )
      )
    )
  )

(defun window-ring-shrink-sides (amt)
  (interactive "NShrink By: ")
  (with-window-ring
      (let ((curr (selected-window)))
        (walk-windows #'(lambda (wind)
                          (when (not (eq wind curr))
                            (with-selected-window wind
                              (shrink-window-horizontally amt))))
                      )))
  )

;;-- end display

;;-- edit

(defun window-ring-edit-order ()
  (interactive)
  (with-window-ring
      (let ((buffers (ring-elements wr-actual))
            (edit-buffer (get-buffer-create (format "*WR Buffers: %s*" (persp-parameter 'name))))
            )
        (with-current-buffer edit-buffer
          (auto-save-mode -1)
          (window-ring-edit-minor-mode 1)
          (set (make-local-variable 'backup-inhibited) t)
          (erase-buffer)
          (mapc (lambda (x) (insert (format "%s\n" (buffer-name x))))
                (reverse buffers))
          )
        (display-buffer edit-buffer)
        )
    )
  )

(defun window-ring-edit-commit ()
  (interactive)
  (with-window-ring
      (let ((order (s-split "\n" (buffer-substring-no-properties
                                  (point-min) (point-max)) t)))
        (modify-persp-parameters `((window-ring-actual . ,(make-ring 1))
                                   (window-ring-focus . 0))
                                 )
        (cl-loop for name in order
                 when (get-buffer (string-trim name))
                 do
                 (window-ring-add-to-head (get-buffer (string-trim name)))
                 )
        (kill-buffer-and-window)
        )
    )
  (window-ring-redisplay)
  )

(setq window-ring-edit-map (make-sparse-keymap))

(define-key window-ring-edit-map (kbd "C-c C-c") #'window-ring-edit-commit)

(define-minor-mode window-ring-edit-minor-mode
  " A Minor mode to commit changes to the order of window ring buffers "
  :lighter "Window-Ring-Edit"
  :keymap window-ring-edit-map
  )
;;-- end edit

(provide 'window-ring-minor-mode)
