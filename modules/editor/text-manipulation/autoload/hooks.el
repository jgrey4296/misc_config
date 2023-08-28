;;; hooks.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +ligature-init-composition-table-h ()
  (dolist (char-regexp +ligatures-composition-alist)
    (set-char-table-range
     +ligature--composition-table
     (car char-regexp) `([,(cdr char-regexp) 0 font-shape-gstring])))
  (set-char-table-parent +ligature--composition-table composition-function-table)
  )

;;;###autoload
(defun doom-init-smartparens-in-eval-expression-h ()
      "Enable `smartparens-mode' in the minibuffer for `eval-expression'.
This includes everything that calls `read--expression', e.g.
`edebug-eval-expression' Only enable it if
`smartparens-global-mode' is on."
      (when smartparens-global-mode (smartparens-mode +1))
      )

;;;###autoload
(defun doom-init-smartparens-in-minibuffer-maybe-h ()
      "Enable `smartparens' for non-`eval-expression' commands.
Only enable `smartparens-mode' if `smartparens-global-mode' is
on."
      (when (and smartparens-global-mode (memq this-command '(evil-ex)))
        (smartparens-mode +1))
      )

;;;###autoload
(defun doom-enable-smartparens-mode-maybe-h ()
      (when doom-buffer-smartparens-mode
        (turn-on-smartparens-mode)
        (kill-local-variable 'doom-buffer-smartparens-mode))
      )

;;;###autoload
(defun doom-disable-smartparens-mode-maybe-h ()
      (when smartparens-mode
        (setq-local doom-buffer-smartparens-mode t)
        (turn-off-smartparens-mode))
      )

;;;###autoload
(defun +hl-todo--use-face-detection-h ()
      "Use a different, more primitive method of locating todo keywords."
      (set (make-local-variable 'hl-todo-keywords)
           '(((lambda (limit)
                (let (case-fold-search)
                  (and (re-search-forward hl-todo-regexp limit t)
                       (memq 'font-lock-comment-face (ensure-list (get-text-property (point) 'face))))))
              (1 (hl-todo-get-face) t t))))
      (when hl-todo-mode
        (hl-todo-mode -1)
        (hl-todo-mode +1)))

;;;###autoload
(defun doom-detect-indentation-h ()
  (unless (or (not after-init-time)
              doom-inhibit-indent-detection
              doom-large-file-p
              (memq major-mode doom-detect-indentation-excluded-modes)
              (member (substring (buffer-name) 0 1) '(" " "*")))
    ;; Don't display messages in the echo area, but still log them
    (let ((inhibit-message (not init-file-debug)))
      (dtrt-indent-mode +1)))
  )


;;;###autoload
(defun +spell-remove-run-together-switch-for-aspell-h ()
  (setq-local ispell-extra-args (remove "--run-together" ispell-extra-args))
  )

;;;###autoload
(defun +spell-init-excluded-faces-h ()
               "Set `spell-fu-faces-exclude' according to `+spell-excluded-faces-alist'."
               (when-let (excluded (cdr (cl-find-if #'derived-mode-p +spell-excluded-faces-alist :key #'car)))
                 (setq-local spell-fu-faces-exclude excluded))
               )

;;;###autoload
(defun +spell-inhibit-duplicate-detection-maybe-h ()
               "Don't mark duplicates when style/grammar linters are present.
e.g. proselint and langtool."
               (and (or (and (bound-and-true-p flycheck-mode)
                             (executable-find "proselint"))
                        (featurep 'langtool))
                    (setq-local flyspell-mark-duplications-flag nil))
               )


;;;###autoload
(defun +format-enable-on-save-maybe-h ()
  "Enable formatting on save in certain major modes.

This is controlled by `+format-on-save-enabled-modes'."
  (or (cond ((eq major-mode 'fundamental-mode))
            ((string-prefix-p " " (buffer-name)))
            ((and (booleanp +format-on-save-enabled-modes)
                  (not +format-on-save-enabled-modes)))
            ((and (listp +format-on-save-enabled-modes)
                  (if (eq (car +format-on-save-enabled-modes) 'not)
                      (memq major-mode (cdr +format-on-save-enabled-modes))
                    (not (memq major-mode +format-on-save-enabled-modes)))))
            ((not (require 'format-all nil t))))
      (format-all-mode +1)))
