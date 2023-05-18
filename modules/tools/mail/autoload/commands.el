;;; commands.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-mail-quit-rmail ()
  (interactive)
  (let ((mail-buff rmail-buffer)
        (summ-buff rmail-summary-buffer))
    (cond (summ-buff
           (with-current-buffer summ-buff
             (rmail-summary-quit)
             )
           )
          (mail-buff
           (with-current-buffer mail-buff
             (rmail-quit)
             )
           )
          )
    (if (buffer-live-p mail-buff)
        (kill-buffer mail-buff))
    (if (buffer-live-p summ-buff)
        (kill-buffer summ-buff))
    )
  )

;;;###autoload
(defun +jg-mail-summary-goto-msg-no-show (n)
  " Set the rmail-buffer to a message, but don't pop to buffer "
  (interactive "n")
  (save-excursion
    (goto-char (point-min))
    (forward-line n)
    (let ((summary-buf (current-buffer))
          (mail-buf rmail-buffer)
          (target-msg (string-to-number
                       (buffer-substring
                        (line-beginning-position)
                        (min (point-max) (+ 6 (line-beginning-position))))))
          beg end body-beg view-buf
          )
      (cond ((not target-msg)
             (message "No Message to select"))
            ((= target-msg 0)
             (message "No Message to select"))
            (t (setq rmail-current-message target-msg)
               (with-current-buffer mail-buf
                 (rmail-swap-buffers)
                 (setq view-buf rmail-view-buffer)
                 (with-current-buffer view-buf (erase-buffer))
                 (widen)
	         (setq beg (rmail-msgbeg target-msg)
	               end (rmail-msgend target-msg))
	         (narrow-to-region beg end)
	         (goto-char (point-min))
                 (setq body-beg (search-forward "\n\n" nil t))
	         (rmail-copy-headers beg end)
                 (with-current-buffer view-buf
                   (goto-char (point-min))
                   (rfc2047-decode-region (point-min) (point-max))
                   (goto-char (point-min))
                   (rmail-highlight-headers)
                   (goto-char (point-max))
                   (insert "\n\n")
                   (insert-buffer-substring mail-buf body-beg end)
                   )
                 ;; (rmail-display-labels)
                 (rmail-swap-buffers)
                 (setq rmail-buffer-swapped t)
                 (run-hooks 'rmail-show-message-hook)
                 )
               )
            )
      (set-buffer summary-buf)
      )
    )
  )

;;;###autoload
(defun +jg-mail-summary-by-labels (label)
  (interactive (list (rmail-read-label "Sort By Label: ")))
  (rmail-summary-by-labels (symbol-name label))
  )

;;;###autoload
(defun +jg-mail-summary-label-by-body-regexp (regexp label)
  (interactive (list (read-string "Regexp: ") (rmail-read-label "Label: ")))
  (save-excursion
    (with-current-buffer rmail-buffer
      (rmail-swap-buffers)
      (goto-char (point-min))
      (while (< (point) (point-max))
        ;; get current bounds
        ;; narrow to body
        ;; search
        ;; ;; apply label if its found
        ;; edit and add the label
        )

      (rmail-swap-buffers)
      )
    ;; update the summary vector
    )
  )

;;;###autoload
(defun +jg-mail-summary-label-by-regexp (regexp label)
  (interactive (list (read-string "Regexp: ") (rmail-read-label "Label: ")))
  (+jg-mail-summary-label-by-test (format "%s" label) "Subject" #'string-match regexp)
  )

;;;###autoload
(defun +jg-mail-summary-remove-label (label)
  (interactive "MLabel: ")
  (+jg-mail-summary-label-by-test label "Subject" #'(lambda (patt val) t) t t)
  )

;;;###autoload
(defun +jg-mail-summary-unlabel-by-regexp (regexp label)
  (interactive "MRegex: \nMLabel: ")
  (+jg-mail-summary-label-by-test label "Subject" #'string-match regexp t)
  )

;;;###autoload
(defun +jg-mail-summary-delete-msg ()
  (interactive)
  (let ((msg (+jg-mail-summary-get-msg-no)))
    (with-current-buffer rmail-buffer
      (rmail-set-message-deleted-p msg t)
      )
    (rmail-summary-mark-deleted msg)
    )
  )

;;;###autoload
(defun +jg-mail-summary-delete-all ()
  (interactive)
  (+jg-mail-summary-delete-by-test "Subject" #'(lambda (patt val) t) t)
)

;;;###autoload
(defun +jg-mail-summary-undelete-all ()
  (interactive)
  (+jg-mail-summary-delete-by-test "Subject" #'(lambda (patt val) t) t t)
)

;;;###autoload
(defun +jg-mail-summary-delete-by-regexp (regexp)
  (interactive "MRegex: ")
  (+jg-mail-summary-delete-by-test "Subject" #'string-match regexp)
  )

;;;###autoload
(defun +jg-mail-summary-undelete-by-regexp (regexp)
  (interactive "MRegex: ")
  (+jg-mail-summary-delete-by-test "Subject" #'string-match regexp t)
  )

;;;###autoload
(defun +jg-mail-summary-delete-by-label (label)
  (interactive (list (rmail-read-label "Delete all mail with Label: ")))
  (+jg-mail-summary-delete-by-test rmail-keyword-header #'string-match (symbol-name label))
  )

;;;###autoload
(defun +jg-mail-summary-undelete-by-label (label)
  (interactive "MLabel: ")
  (+jg-mail-summary-delete-by-test rmail-keyword-header #'string-match label t)
  )

;;;###autoload
(defun +jg-mail-summary-delete-older-than ()
  (interactive)
  (let ((date (org-read-date nil t)))
    (+jg-mail-summary-delete-by-test "Date" #'(lambda (pattern value)
                                                (time-less-p
                                                 (encode-time (parse-time-string value))
                                                 pattern))
                                     date)
    )
  )

;;-- utils

(defun +jg-mail-summary-edit-label (msg-num label state)
  (save-excursion
    (let* ((summary-buff (current-buffer))
           (mail-buff rmail-buffer)
           (inhibit-read-only t)
           msg-beg msg-end
           kwds has-kwd
           )
      (unwind-protect
          ;; Initial swap
          (with-current-buffer mail-buff
            (rmail-swap-buffers)
            (rmail-maybe-set-message-counters)
            (widen)
            (goto-char (rmail-msgbeg msg-num))
            (narrow-to-region (rmail-msgbeg msg-num) (rmail-msgend msg-num))
            (setq kwds (or (mail-fetch-field rmail-keyword-header) "")
                  has-kwd (if (string-match (concat "\\(, \\)?" label "\\(, \\)?") kwds)
                              (list (match-beginning 0) (match-end 0))
                            nil
                            ))
            ;; Update kwds
            (goto-char (point-min))
            (cond ((and has-kwd state) ;; has, doesn't need to insert
                   t
                   )
                  ((and (not has-kwd) (not state)) ;; doesnt have, doesnt need
                   t
                   )
                  (has-kwd ;; Remove
                   (let ((lbounds `(,(min 0 (car has-kwd))
                                    . ,(max 0 (car has-kwd))))
                         (rbounds  `(,(min (cadr has-kwd) (length kwds))
                                    . ,(max (cadr has-kwd) (length kwds)))))
                     (rmail-set-header-1 rmail-keyword-header
                                         (concat (s-trim (substring kwds (car lbounds) (cdr lbounds)))
                                                 (if (> 0 (cdr lbounds)) ", ")
                                                 (s-trim (substring kwds (car rbounds) (cdr rbounds)))))
                     )
                   )
                  (state ;; insert
                   (rmail-set-header-1 rmail-keyword-header (concat kwds ", " label))
                   )
                  )
            )
        ;; Swap back:
        (with-current-buffer mail-buff
          (rmail-swap-buffers)
          )
        )
      )
    )
  )

(defun +jg-mail-summary-label-by-test (label target test-fn pattern &optional unlabel)
  " (Un)Delete by getting the target from each message
 and testing it in test-fn::(lambda pattern value)
 "
  (let (updates)
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let* ((msg-num (+jg-mail-summary-get-msg-no))
               (labels (or (rmail-get-header rmail-keyword-header msg-num) ""))
               (has-label (string-match label labels))
               )
          (cond ((and unlabel has-label
                      (apply test-fn pattern (or (rmail-get-header target msg-num) "") nil))
                 (+jg-mail-summary-edit-label msg-num label nil)
                 (push msg-num updates)
                 )
                (unlabel
                 ;; unlabel but it doesn't matter, do nothing
                 nil
                 )
                ((and (not has-label) (apply test-fn pattern (or (rmail-get-header target msg-num) "") nil))
                 (+jg-mail-summary-edit-label msg-num label t)
                 (rmail-summary-add-label label)
                 (push msg-num updates)
                 )
                (t nil)
                )
          (forward-line)
          )
        )
      )
    (with-current-buffer rmail-buffer
      (mapc (lambda (x)
              (aset rmail-summary-vector (1- x) nil))
            updates))
    (mapc #'rmail-summary-update-line updates)
    )
  )

(defun +jg-mail-summary-delete-by-test (target test-fn pattern &optional undelete)
  " (Un)Delete by getting the target from each message
 and testing it in test-fn::(lambda pattern value)
 "
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let* ((msg-num (+jg-mail-summary-get-msg-no))
             (is-deleted (with-current-buffer rmail-buffer
                           (rmail-message-deleted-p msg-num)))
             )
        (cond ((and undelete (not is-deleted))
               t)
              ((apply test-fn pattern (or (rmail-get-header target msg-num) "") nil)
               (with-current-buffer rmail-buffer
                 (rmail-set-message-deleted-p msg-num (not undelete))
                 (run-hooks 'rmail-delete-message-hook))
               )
              (t nil)
              )
        )
      (forward-line)
      )
    ;; Processed everything, now update summary
    (let ((deleted (with-current-buffer rmail-buffer rmail-deleted-vector)))
      (mapc #'(lambda (x)
                (rmail-summary-mark-deleted x (and undelete
                                                   (not (= (aref deleted x) ?D)))))
            (number-sequence 1 rmail-total-messages))
      )
    (rmail-update-summary)
    )
  )

(defun +jg-mail-summary-get-msg-no ()
  (let ((target-msg (string-to-number
                     (buffer-substring
                      (line-beginning-position)
                      (min (point-max) (+ 6 (line-beginning-position))))))
        )
    target-msg
    )
  )

;;-- end utils
