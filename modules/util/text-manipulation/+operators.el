;;; +operators.el -*- lexical-binding: t; -*-

(evil-define-operator +jg-text-encrypt-region (beg end type)
  " Operator to easily envcrypt a region of text "
  :type line
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

(evil-define-operator +jg-text-decrypt-region (beg end type)
  " Operator to easily envcrypt a region of text "
  :type line
  (interactive "<R>")
  (let* ((encrypted (buffer-substring-no-properties beg end))
         (context (epg-make-context epa-protocol))
         decrypted)
    ;; (decrypted (epg-decrypt-string (epg-make-context epa-protocol)
    ;;                                (buffer-substring-no-properties beg end))))
    (let ((input-file (make-temp-file "epg-input"))
          (output-file (make-temp-file "epg-output"))
          (coding-system-for-write 'binary))
      (unwind-protect
          (progn
            (write-region encrypted nil input-file nil 'quiet)
            (setf (epg-context-output-file context) output-file)
            (epg-start-decrypt context (epg-make-data-from-file input-file))
            (epg-wait-for-completion context)
            (epg--check-error-for-decrypt context)
            (setq decrypted (epg-read-output context)))
        (epg-delete-output-file context)
        (if (file-exists-p input-file)
            (delete-file input-file))
        (if (file-exists-p output-file)
            (delete-file output-file))
        (epg-reset context)))

    (save-excursion
      (goto-char beg)
      (kill-region beg end)
      (insert decrypted)
      )
    )
  )

(evil-define-operator +jg-text-wrap-fold-block (beg end type &optional name)
  " Operator to easily create fold blocks "
  :type line
  :keep-visual t
  (interactive "<R>" (list (read-string "Block Name: ")))
  ;; From bottom to top to not change interfere with positions
  ;; add end fold block
  (goto-char end)
  (end-of-line)
  (insert (+jg-text-fold-block-gen :name name :end t :newlines t))
  ;; and start fold block
  (goto-char beg)
  (beginning-of-line)
  (insert (+jg-text-fold-block-gen :name name :newlines t))
  )

(evil-define-operator +jg-text-make-invisible (beg end type)
  " Operator to easily annotate text to be hidden "
  :type inclusive
  (interactive "<R>")
  (put-text-property beg end 'invisible 'jg-text-invis)
  )

(evil-define-operator +jg-text-toggle-invisible (beg end type prefix)
  " Operator to show invisible text again "
  :type inclusive
  :keep-visual t
  (interactive "<R>p")
  ;; Toggle the property
  (alter-text-property beg end 'invisible
                       (lambda (val)
                         (cond ((eq val 'jg-text-invis)
                                'jg-text-invis-disabled
                                )
                               ((eq val 'jg-text-invis-disabled)
                                'jg-text-invis)
                               (t val)
                               )
                         )
                       )
  )

(defun +jg-text-toggle-invisible-spec ()
  (interactive)
  (cond ((assoc 'jg-text-invis buffer-invisibility-spec)
         (setq buffer-invisibility-spec (assq-delete-all 'jg-text-invis buffer-invisibility-spec)))
        (t
         (push '(jg-text-invis . t) buffer-invisibility-spec)
         )
        )
  )

(cl-defun +jg-text-fold-block-gen (&rest rst &key name (end nil) (re nil) (newlines nil) (comment comment-start))
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
