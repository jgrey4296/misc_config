;;; advice.el -*- lexical-binding: t; -*-

(defun +popup--split-window (window size side)
  "Ensure a non-dedicated/popup window is selected when splitting a window."
  (unless +popup--internal
    (cl-loop for win
             in (cons (or window (selected-window))
                      (window-list nil 0 window))
             unless (+popup-window-p win)
             return (setq window win)))
  (let ((ignore-window-parameters t))
    (split-window window size side)))

;;;###autoload
(defun +popup-close-a (&rest _)
  "TODO"
  (+popup/close nil t))

;;;###autoload
(defun +popup-display-buffer-fullframe-fn (buffer alist)
  "Displays the buffer fullscreen."
  (let ((wconf (current-window-configuration)))
    (when-let (window (or (display-buffer-reuse-window buffer alist)
                          (display-buffer-same-window buffer alist)
                          (display-buffer-pop-up-window buffer alist)
                          (display-buffer-use-some-window buffer alist)))
      (set-window-parameter window 'saved-wconf wconf)
      (add-to-list 'window-persistent-parameters '(saved-wconf . t))
      (delete-other-windows window)
      window)))

;;;###autoload
(defun +popup-display-buffer-stacked-side-window-fn (buffer alist)
  "A `display-buffer' action that serves as an alternative to
`display-buffer-in-side-window', but allows for stacking popups with the `vslot'
alist entry.

Accepts the same arguments as `display-buffer-in-side-window'. You must set
`window--sides-inhibit-check' to non-nil for this work properly."
  (let* ((side  (or (cdr (assq 'side alist)) 'bottom))
         (slot  (or (cdr (assq 'slot alist))  0))
         (vslot (or (cdr (assq 'vslot alist)) 0))
         (left-or-right (memq side '(left right)))
         (display-buffer-mark-dedicated (or display-buffer-mark-dedicated 'popup)))

    (cond ((not (memq side '(top bottom left right)))
           (error "Invalid side %s specified" side))
          ((not (numberp slot))
           (error "Invalid slot %s specified" slot))
          ((not (numberp vslot))
           (error "Invalid vslot %s specified" vslot)))

    (let* ((major (get-window-with-predicate
                   (lambda (window)
                     (and (eq (window-parameter window 'window-side) side)
                          (eq (window-parameter window 'window-vslot) vslot)))
                   nil))
           (reversed (window--sides-reverse-on-frame-p (selected-frame)))
           (windows
            (cond ((window-live-p major)
                   (list major))
                  ((window-valid-p major)
                   (let* ((first (window-child major))
                          (next (window-next-sibling first))
                          (windows (list next first)))
                     (setq reversed (> (window-parameter first 'window-slot)
                                       (window-parameter next 'window-slot)))
                     (while (setq next (window-next-sibling next))
                       (setq windows (cons next windows)))
                     (if reversed windows (nreverse windows))))))
           (slots (if major (max 1 (window-child-count major))))
           (max-slots
            (nth (plist-get '(left 0 top 1 right 2 bottom 3) side)
                 window-sides-slots))
           (window--sides-inhibit-check t)
           window this-window this-slot prev-window next-window
           best-window best-slot abs-slot)

      (cond ((and (numberp max-slots) (<= max-slots 0))
             nil)
            ((not windows)
             (cl-letf (((symbol-function 'window--make-major-side-window-next-to)
                        (lambda (_side) (frame-root-window (selected-frame)))))
               (when-let (window (window--make-major-side-window buffer side slot alist))
                 (set-window-parameter window 'window-vslot vslot)
                 (add-to-list 'window-persistent-parameters '(window-vslot . writable))
                 window)))
            (t
             ;; Scan windows on SIDE.
             (catch 'found
               (dolist (window windows)
                 (setq this-slot (window-parameter window 'window-slot))
                 (cond ((not (numberp this-slot)))
                       ((= this-slot slot) ; A window with a matching slot found
                        (setq this-window window)
                        (throw 'found t))
                       (t
                        ;; Check if this window has a better slot value wrt the
                        ;; slot of the window we want.
                        (setq abs-slot
                              (if (or (and (> this-slot 0) (> slot 0))
                                      (and (< this-slot 0) (< slot 0)))
                                  (abs (- slot this-slot))
                                (+ (abs slot) (abs this-slot))))
                        (unless (and best-slot (<= best-slot abs-slot))
                          (setq best-window window)
                          (setq best-slot abs-slot))
                        (if reversed
                            (cond
                             ((<= this-slot slot)
                              (setq next-window window))
                             ((not prev-window)
                              (setq prev-window window)))
                          (cond
                           ((<= this-slot slot)
                            (setq prev-window window))
                           ((not next-window)
                            (setq next-window window))))))))

             ;; `this-window' is the first window with the same SLOT.
             ;; `prev-window' is the window with the largest slot < SLOT. A new
             ;; window will be created after it.
             ;; `next-window' is the window with the smallest slot > SLOT. A new
             ;; window will be created before it.
             ;; `best-window' is the window with the smallest absolute
             ;; difference of its slot and SLOT.
             (or (and this-window
                      ;; Reuse `this-window'.
                      (with-current-buffer buffer
                        (setq window--sides-shown t))
                      (window--display-buffer
                       buffer this-window 'reuse alist))
                 (and (or (not max-slots) (< slots max-slots))
                      (or (and next-window
                               ;; Make new window before `next-window'.
                               (let ((next-side (if left-or-right 'above 'left))
                                     (+popup--internal t)
                                     (window-combination-resize 'side))
                                 (setq window
                                       (ignore-errors (split-window next-window nil next-side)))))
                          (and prev-window
                               ;; Make new window after `prev-window'.
                               (let ((prev-side (if left-or-right 'below 'right))
                                     (+popup--internal t)
                                     (window-combination-resize 'side))
                                 (setq window
                                       (ignore-errors (split-window prev-window nil prev-side))))))
                      (set-window-parameter window 'window-slot slot)
                      (with-current-buffer buffer
                        (setq window--sides-shown t))
                      (window--display-buffer
                       buffer window 'window alist))
                 (and best-window
                      ;; Reuse `best-window'.
                      (progn
                        ;; Give best-window the new slot value.
                        (set-window-parameter best-window 'window-slot slot)
                        (with-current-buffer buffer
                          (setq window--sides-shown t))
                        (window--display-buffer
                         buffer best-window 'reuse alist)))))))))
