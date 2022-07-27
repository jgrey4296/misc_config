;;; util/jg-mail/+funcs.el -*- lexical-binding: t; -*-

(defun +jg-mail-override-mu4e-hook ()
  (message "Mail override hook: %s" (current-time-string))
  ;; (setq-default smtpmail-smtp-service 465
  ;;               message-send-mail-function   'smtpmail-send-it
  ;;               smtpmail-default-smtp-server "smtp.gmail.com"
  ;;               smtpmail-smtp-server         "smtp.gmail.com"
  ;;               smtpmail-stream-type  'starttls
  ;;               smtpmail-smtp-service 587
  ;;               auth-sources '("~/.authinfo"
  ;;                              macos-keychain-generic
  ;;                              macos-keychain-internet
  ;;                              "~/authinfo.gpg")
  ;;               )

  (setq mu4e-use-fancy-chars nil
        mu4e-headers-draft-mark     '("D" . "D")
        mu4e-headers-flagged-mark   '("F" . "F")
        mu4e-headers-new-mark       '("N" . "N")
        mu4e-headers-passed-mark    '("P" . "P")
        mu4e-headers-replied-mark   '("R" . "R")
        mu4e-headers-seen-mark      '("S" . "S")
        mu4e-headers-trashed-mark   '("T" . "T")
        mu4e-headers-attach-mark    '("a" . "a")
        mu4e-headers-encrypted-mark '("x" . "x")
        mu4e-headers-signed-mark    '("s" . "s")
        mu4e-headers-unread-mark    '("u" . "u"))

  (setq auth-source-backend-parser-functions
        '(auth-source-backends-parser-secrets
          auth-source-backends-parser-file))
)

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

(defun +jg-mail-summary-delete-msg ()
  (interactive)
  (let ((msg (+jg-mail-summary-get-msg-no)))
    (with-current-buffer rmail-buffer
      (rmail-set-message-deleted-p msg t)
      )
    (rmail-summary-mark-deleted msg)
    )
  )

(defun +jg-mail-summary-delete-all ()
  (interactive)
  (with-current-buffer rmail-buffer
    (setq rmail-deleted-vector (concat " " (make-string rmail-total-messages ?D)))
    )
  (loop for x from 1 to rmail-total-messages
        do (rmail-summary-mark-deleted x)
        )
  (rmail-update-summary)
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

(defun +jg-mail-summary-delete-by-regex (regexp)
  (interactive "MRegex: ")
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let* ((msg-num (+jg-mail-summary-get-msg-no))
             (subj (rmail-get-header "Subject" msg-num))
             )
        (if (string-match regexp subj)
            (with-current-buffer rmail-buffer
              (rmail-set-message-deleted-p msg-num t)
              (run-hooks 'rmail-delete-message-hook)
              )
          )
        (forward-line)
        )
      )
    (let ((deleted (with-current-buffer rmail-buffer
                     rmail-deleted-vector)))
      (mapc (lambda (x)
              (if (= (aref deleted x) ?D)
                  (rmail-summary-mark-deleted x)))
            (number-sequence 1 rmail-total-messages)))
    (rmail-update-summary)
    )
  )

(defun +jg-mail-summary-undelete-by-regex (regexp)
  (interactive "MRegex: ")
  (save-excursion
    (let* ((summary-buf (current-buffer))
           (mail-buf rmail-buffer)
           (inhibit-read-only t)
           msg-num msg-beg msg-end
           updates
           )
      (unwind-protect
          (with-current-buffer rmail-buffer
            (rmail-swap-buffers)
            (rmail-maybe-set-message-counters)
            (widen)
            (cl-loop for x from 1 to rmail-total-messages
                     do
                     (if (= (aref rmail-deleted-vector x) ?D)
                         (progn
                           (setq msg-beg (rmail-msgbeg x)
                                 msg-end (rmail-msgend x)
                                 rmail-current-message x)
                           (narrow-to-region msg-beg msg-end)
                           (goto-char (point-min))
                           (if (re-search-forward regexp nil t)
                               (progn (goto-char (point-min))
                                      (rmail-set-attribute-1 rmail-deleted-attr-index nil)
                                      (rmail-set-message-deleted-p x nil)
                                      (push x updates)
                                      )
                             )
                           )
                       )
                     )
            )
      ;; Swap back:
      (with-current-buffer rmail-buffer
        (rmail-swap-buffers)
        )
      (mapc #'rmail-summary-mark-undeleted updates)
      (rmail-maybe-display-summary)
      )
      )
    )
  )

(defun +jg-mail-summary-delete()
  " Delete a message without reshowing the rmail buffer "
  (interactive)
  (with-current-buffer rmail-buffer
    (setq del-msg rmail-current-message)
    (rmail-delete-message)
    )
  (rmail-summary-mark-deleted del-msg)
  )

(defun +jg-mail-summary-label-by-regex (regexp label)
  (interactive "MRegex: \nMLabel: ")
  (save-excursion
    (let* ((summary-buf (current-buffer))
           (mail-buf rmail-buffer)
           (inhibit-read-only t)
           msg-num msg-beg msg-end
           kwds updates
           )
    (unwind-protect
        ;; Initial swap
        (with-current-buffer rmail-buffer
          (rmail-swap-buffers)
          (rmail-maybe-set-message-counters)
          (widen)
          (cl-loop for x from 1 to rmail-total-messages
                   do
                   (setq msg-beg (rmail-msgbeg x)
                         msg-end (rmail-msgend x)
                         rmail-current-message x)
                   (narrow-to-region msg-beg msg-end)
                   (goto-char (point-min))
                   (setq kwds (or (mail-fetch-field rmail-keyword-header)
                                  ""))
                   (goto-char (point-min))
                   (if (and (null (string-match label kwds))
                            (re-search-forward regexp nil t))
                       (progn (goto-char (point-min))
                              (rmail-set-header-1 rmail-keyword-header
                                                  (if (string-empty-p kwds)
                                                      label
                                                    (concat kwds ", " label)))
                              (push x updates)
                              )
                     )
                   )
          )
      ;; Swap back:
      (with-current-buffer rmail-buffer
        (rmail-swap-buffers)
        (mapc (lambda (x)
                (aset rmail-summary-vector (1- x) nil))
              updates))
      (mapc #'rmail-summary-update-line updates)
      )
    )
    )
  )

(defun +jg-mail-summary-unlabel-by-regex (regexp label)
  (interactive "MRegex: \nMLabel: ")
  (save-excursion
    (let* ((summary-buf (current-buffer))
           (mail-buf rmail-buffer)
           (inhibit-read-only t)
           msg-num msg-beg msg-end
           kwds updates
           )
    (unwind-protect
        ;; Initial swap
        (with-current-buffer rmail-buffer
          (rmail-swap-buffers)
          (widen)
          (cl-loop for x from 1 to rmail-total-messages
                   do
                   (setq msg-beg (rmail-msgbeg x)
                         msg-end (rmail-msgend x)
                         rmail-current-message x)
                   (narrow-to-region msg-beg msg-end)
                   (goto-char (point-min))
                   (setq kwds (or (mail-fetch-field rmail-keyword-header)
                                  ""))
                   (goto-char (point-min))
                   (if (and (not (null (string-match label kwds)))
                            (re-search-forward regexp nil t))
                       (progn (goto-char (point-min))
                              (rmail-set-header-1 rmail-keyword-header
                                                  (if (s-equals? kwds label)
                                                      ""
                                                    (s-replace-regexp ", ,"
                                                                      ""
                                                                      (s-replace-regexp label
                                                                                        ""
                                                                                        kwds))))
                              (push x updates)
                              )
                     )
                   )
          )
      ;; Swap back:
      (with-current-buffer rmail-buffer
        (rmail-swap-buffers)
        (mapc (lambda (x)
                (aset rmail-summary-vector (1- x) nil))
              updates))
      (mapc #'rmail-summary-update-line updates)
      )
    )
    )
  )
