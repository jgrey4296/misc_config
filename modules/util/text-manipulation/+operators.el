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
