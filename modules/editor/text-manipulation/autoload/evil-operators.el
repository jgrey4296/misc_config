;;; +operators.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+jg-text-encrypt-region "editor/text-manipulation/autoload/evil-operators" nil t)
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

;;;###autoload (autoload '+jg-text-decrypt-region "editor/text-manipulation/autoload/evil-operators" nil t)
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

;;;###autoload (autoload '+jg-text-toggle-invisible-spec "editor/text-manipulation/autoload/evil-operators" nil t)
(defun +jg-text-toggle-invisible-spec ()
  (interactive)
  (cond ((and (listp buffer-invisibility-spec) (assoc 'jg-text-invis buffer-invisibility-spec))
         (setq buffer-invisibility-spec (assq-delete-all 'jg-text-invis buffer-invisibility-spec)))
        ((listp buffer-invisibility-spec)
         (push '(jg-text-invis . t) buffer-invisibility-spec)
         )
        (t
         (setq buffer-invisibility-spec (cons '(jg-text-invis . t) buffer-invisibility-spec))
         )
        )
  )

;;;###autoload (autoload '+jg-text-make-invisible "editor/text-manipulation/autoload/evil-operators" nil t)
(evil-define-operator +jg-text-make-invisible (beg end type)
  " Operator to easily annotate text to be hidden "
  :type inclusive
  (interactive "<R>")
  (put-text-property beg end 'invisible 'jg-text-invis)
  )

;;;###autoload (autoload '+jg-text-delete-invisible "editor/text-manipulation/autoload/evil-operators" nil t)
(evil-define-operator +jg-text-delete-invisible (beg end type)
  " Operator to easily annotate text to be hidden "
  :type inclusive
  (interactive "<R>")
  (put-text-property beg end 'invisible nil)
  )

;;;###autoload (autoload '+jg-text-toggle-invisible "editor/text-manipulation/autoload/evil-operators" nil t)
(evil-define-operator +jg-text-toggle-invisible (beg end type prefix)
  " Operator to show invisible text again "
  :type inclusive
  ;; :keep-visual t
  (interactive "<R>P")
  ;; Toggle the property
  (alter-text-property beg end 'invisible
                       (lambda (val)
                         (cond (prefix nil)
                               ((eq val 'jg-text-invis)
                                'jg-text-invis-disabled
                                )
                               ((eq val 'jg-text-invis-disabled)
                                'jg-text-invis)
                               (t 'jg-text-invis)
                               )
                         )
                       )
  )

;;;###autoload (autoload '+jg-text-split-on-char-op "editor/text-manipulation/autoload/evil-operators" nil t)
(evil-define-operator +jg-text-split-on-char-op (beg end)
  ;; TODO
  :move-point t
  (message "Count: %s" count)
  (let ((last-char (downcase (char-after (+ (point) count))))
        curr-char)
    (while (< (point) end)
      (setq curr-char (downcase (char-after (+ (point) count))))
      (if (not (eq last-char curr-char))
          (progn
            (setq last-char curr-char)
            (goto-char (line-beginning-position))
            (insert "\n")
            )
        )
      (forward-line)
      )
    )
  )

;;;###autoload (autoload '+jg-text-remove-leading-whitespace-op "editor/text-manipulation/autoload/evil-operators" nil t)
(evil-define-operator +jg-text-remove-leading-whitespace-op (beg end count)
  :move-point t
  (while (< (point) end)
    (evil-first-non-blank)
    (delete-region (line-beginning-position) (point))
    (forward-line)
    )
  )

;;;###autoload (autoload '+jg-text-uniquify-op "editor/text-manipulation/autoload/evil-operators" nil t)
(evil-define-operator +jg-text-uniquify-op (beg end count)
  :move-point t
  (evil-first-non-blank)
  (let ((R-mark (set-marker (make-marker) end))
        (current (+jg-text-get-line))
        (kill-whole-line t))
    (forward-line 1)
    (while (<= (point) R-mark)
      ;; compare
      (if (s-equals? current (+jg-text-get-line))
          (kill-line)
        (progn (setq current (+jg-text-get-line))
               (forward-line 1))
        )
      )
    )
  )

;;;###autoload (autoload '+jg-text-escalate-replace-op "editor/text-manipulation/autoload/evil-operators" nil t)
(evil-define-operator +jg-text-escalate-replace-op (beg end count)
  " Replace a regex in the region,
with either a numeric or alphabetical escalation "
  :move-point t
  (let* ((end-mark (set-marker (make-marker) end))
         (reg  (read-string "Regexp: " "^.*?\\( \\)"))
         (base (read-string "Replacement Base: "))
         (type (car (read-multiple-choice "Increment Digit or Char? "
                                          '((?d "Digit")
                                            (?c "Char")))))
         (current (if (eq ?d type) 0 ?a))
         )
    (goto-char beg)
    (while (re-search-forward reg end-mark t)
      (replace-match (format "%s_%s" base (if (eq ?c type) (char-to-string current) current))
                     t nil nil 1)
      (cl-incf current)
      (if (and (eq type ?c) (> current ?z)) (setq current ?a))
      )
    )
  )

;;;###autoload (autoload '+jg-text-title-case-op "editor/text-manipulation/autoload/evil-operators" nil t)
(evil-define-operator +jg-text-title-case-op (beg end)
  :move-point t
  (while (< (point) end)
    (capitalize-word 1)
    )
  )

;;;###autoload (autoload '+jg-text-simple-grep-op "editor/text-manipulation/autoload/evil-operators" nil t)
(evil-define-operator +jg-text-simple-grep-op (beg end count)
  :move-point t
  (interactive)
  (let ((reg (read-string "Match Regexp: ")))
    (with-temp-buffer-window "*Text-Results*" 'display-buffer-pop-up-window nil
      (goto-char (point-min))
      (while (re-search-forward reg end t)
        (princ (format "%s : %s\n" (line-number-at-pos) (buffer-substring (line-beginning-position) (line-end-position))))
        )
      )
    )
  (let ((inhibit-read-only t))
    (with-current-buffer "*Text-Results*"
      (align-regexp (point-min) (point-max) "\\(\s-*\\):")
      )
    )
  )

;;;###autoload (autoload '+jg-text-line-on-char-op "editor/text-manipulation/autoload/evil-operators" nil t)
(evil-define-operator +jg-text-line-on-char-op (beg end count)
  (warn 'todo)
  )

;;;###autoload (autoload '+jg-text-goto-random-line-op "editor/text-manipulation/autoload/evil-operators" nil t)
(evil-define-operator +jg-text-goto-random-line-op (beg end)
  :motion +evil:whole-buffer-txtobj
  :repeat t
  :move-point t
  (let* ((min-line (line-number-at-pos beg))
         (max-line (- (line-number-at-pos end) min-line))
         )
    (forward-line (random max-line))
    (evil-first-non-blank-of-visual-line)
    )
  )

;;;###autoload (autoload '+jg-text-shift-left "editor/text-manipulation/autoload/evil-operators" nil t)
(evil-define-operator +jg-text-shift-left (beg end count)
  "Shift text left, preserving state"
  :type line
  :motion beginning-of-visual-line
  :repeat t
  :keep-visual t
  (interactive "<r><vc>")
  (let ((state evil-state))
    (evil-shift-left beg end count t)
    (when (eq state 'visual)
      (evil-normal-state)
      (evil-visual-restore))
    )
  )

;;;###autoload (autoload '+jg-text-shift-right "editor/text-manipulation/autoload/evil-operators" nil t)
(evil-define-operator +jg-text-shift-right (beg end count)
  "shift text right, preserving state"
  :type line
  :motion beginning-of-visual-line
  :repeat t
  :keep-visual t
  (interactive "<r><vc>")
  (let ((state evil-state))
    (evil-shift-right beg end count t)
    (when (eq state 'visual)
      (evil-normal-state)
      (evil-visual-restore))
    )
  )

;;;###autoload (autoload '+jg-text-inc-num  "editor/text-manipulation/autoload/evil-operators" nil t)
(evil-define-operator +jg-text-inc-num (beg end type count)
  :type line
  :motion beginning-of-visual-line
  :repeat t
  :keep-visual t
  (interactive "<v><vc>")
  (let ((state evil-state))
    (if (not (eq state 'visual))
        (evil-numbers/inc-at-pt (or count 1) beg)
      (evil-numbers/inc-at-pt count beg end 'block)
      (evil-normal-state)
      (evil-visual-restore))
    )
  )

;;;###autoload (autoload '+jg-text-dec-num "editor/text-manipulation/autoload/evil-operators" nil t)
(evil-define-operator +jg-text-dec-num (beg end type count)
  :type line
  :motion beginning-of-visual-line
  :repeat t
  :keep-visual t
  (interactive "<v><vc>")
    (let ((state evil-state))
      (if (not (eq state 'visual))
          (evil-numbers/dec-at-pt (or count 1) beg)
        (evil-numbers/dec-at-pt count beg end 'block)
        (evil-normal-state)
        (evil-visual-restore))
      )
  )