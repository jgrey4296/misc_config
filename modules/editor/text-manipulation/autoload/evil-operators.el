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
        (setq current (+jg-text-get-line))
        (forward-line 1)
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
    (if (looking-at " \\<and\\>")
        (forward-word 1)
      (capitalize-word 1)
      )
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

;;;###autoload (autoload '+format:region "editor/format/autoload/evil" nil t)
(evil-define-operator +format:region (beg end)
  "Evil ex interface to `+format/region'."
  (interactive "<r>")
  (+format/region beg end))

;;;###autoload (autoload '+jg-surround-list "editor/text-manipulation/autoload/list-surround.el" nil t)
(evil-define-operator +jg-surround-list (beg end args)
  :type inclusive
  :motion nil
  (interactive "<R>")
  (let* ((list-pair (evil-surround-pair (read-char "List Type: "))))
    (save-excursion
      (goto-char end)
      (if (looking-at-p "\n")
          (backward-char 1))
      (insert (cdr list-pair))
      (goto-char beg)
      (insert (car list-pair))
      (unless (derived-mode-p 'emacs-lisp-mode)
        (+jg-surround-ensure-commas beg end args))
      )
    )
  )

;;;###autoload (autoload '+jg-surround-ensure-commas  "editor/text-manipulation/autoload/list-surround.el" nil t)
(evil-define-operator +jg-surround-ensure-commas (beg end args)
  :type inclusive
  :keep-visual t
  (interactive "<R>")
  (save-excursion
    (goto-char (max beg end))
    ;; look for blanks and new lines, add commas in between
    (while (re-search-backward (rx word-end (opt (| ?\" ?')) (group (| "\n" blank))) (min beg end) t)
      (goto-char (match-beginning 1))
      (unless (nth 3 (syntax-ppss)) ;; if in string
        (insert ","))
      (goto-char (match-beginning 0))
      )
    )
  )
