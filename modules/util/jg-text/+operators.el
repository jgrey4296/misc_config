;;; +operators.el -*- lexical-binding: t; -*-

(evil-define-operator +jg-text-encrypt-region (beg end count)
  " Operator to easily envcrypt a region of text "
  :type exclusive
  (interactive "<R>")
  (let* ((encrypted (epg-encrypt-string (epg-make-context epa-protocol epa-armor)
                             (buffer-substring-no-properties beg end) "")))
    (save-excursion
      (goto-char beg)
      (kill-region beg end)
      (insert encrypted)
      )
    )
  )

(evil-define-operator +jg-text-decrypt-region (beg end count)
  " Operator to easily envcrypt a region of text "
  :type exclusive
  (interactive "<R>")
  (let* ((encrypted (buffer-substring-no-properties beg end))
         (context (epg-make-context epa-protocol))
         decrypted)
         ;; (decrypted (epg-decrypt-string (epg-make-context epa-protocol)
         ;;                                (buffer-substring-no-properties beg end))))
    (let ((input-file (make-temp-file "epg-input"))
	  (coding-system-for-write 'binary))
      (unwind-protect
	  (progn
	    (write-region encrypted nil input-file nil 'quiet)
	    (setf (epg-context-output-file context)
                  (make-temp-file "epg-output"))
	    (epg-start-decrypt context (epg-make-data-from-file input-file))
	    (epg-wait-for-completion context)
	    (epg--check-error-for-decrypt context)
	    (setq decrypted (epg-read-output context))
            (epg-delete-output-file context)
        (if (file-exists-p input-file)
	    (delete-file input-file))
        (epg-reset context))))

    (save-excursion
      (goto-char beg)
      (kill-region beg end)
      (insert decrypted)
      )
    )
  )


(evil-define-operator +jg-wrap-fold-block (beg end count &optional name)
  " Operator to easily create fold blocks "
  :type block
  :keep-visual t
  (interactive "<R>" (list (read-string "Block Name: ")))
  ;; From bottom to top to not change interfere with positions
  ;; add end fold block
  (goto-char end)
  (end-of-line)
  (insert (+jg-fold-block-gen :name name :end t :newlines t))
  ;; and start fold block
  (goto-char beg)
  (beginning-of-line)
  (insert (+jg-fold-block-gen :name name :newlines t))
  )

(cl-defun +jg-fold-block-gen (&rest rst &key name (end nil) (re nil) (newlines nil) (comment comment-start))
  " Single point to build fold block markers
Atuo-recognizes the current major-mode's commment syntax
 "
  (let* ((comment-str (apply 'concat (make-list jg-fold-block-depth (s-trim comment))))
         (end-str (if end "end " nil))
         (name-pattern "\\(.+\\)")
         (name-form (s-concat end-str (if name name name-pattern)))
         (full-pattern (format jg-fold-block-pattern comment-str name-form))
         )
    (cond ((and re newlines) (error "Fold Block Invalid Combined Args: :re and :newlines"))
          ((and newlines (not name)) (error ":newlines should also have :name"))
          (re       (s-concat "^" full-pattern))
          (newlines (s-concat (if end "\n" "") full-pattern "\n"))
          (t full-pattern))))
