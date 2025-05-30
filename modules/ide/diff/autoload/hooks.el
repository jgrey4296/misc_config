;;; hooks.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +vc-gutter-update-h (&rest _)
      "Return nil to prevent shadowing other `doom-escape-hook' hooks."
      (ignore (or inhibit-redisplay
                  (and (or (bound-and-true-p diff-hl-mode)
                           (bound-and-true-p diff-hl-dir-mode))
                       (diff-hl-update-once))))
      )

;;;###autoload
(defun +vc-gutter-init-flydiff-mode-h ()
  " UX: Don't delete the current hunk's indicators while we're editing"
  (if (not diff-hl-flydiff-mode)
      (remove-hook 'evil-insert-state-exit-hook #'diff-hl-flydiff-update)
    (add-hook 'evil-insert-state-exit-hook #'diff-hl-flydiff-update))
  )

;;;###autoload
(defun +vc-gutter-type-face-fn (type _pos)
  (intern (format "diff-hl-%s" type)))

;;;###autoload
(defun +vc-gutter-fix-diff-hl-faces-h ()
  (mapc (doom-rpartial #'set-face-background nil)
        '(diff-hl-insert
          diff-hl-delete
          diff-hl-change))
  )

;;;###autoload
(defun +jg-diff-cleanup-temp-buffers ()
  (cl-loop for buff in (list ediff-buffer-A ediff-buffer-B ediff-buffer-C)
           do
           (when (and (buffer-file-name buff)
                      (f-descendant-of? (buffer-file-name buff) temporary-file-directory))
             (message "Is Temp: %s" buff)
             (kill-buffer buff)
             )
           )
  )

;;;###autoload
(defun doom-ediff-save-wconf-h ()
  (setq doom--ediff-saved-wconf (current-window-configuration))
  )

;;;###autoload
(defun doom-ediff-restore-wconf-h ()
  (when (window-configuration-p doom--ediff-saved-wconf)
    (set-window-configuration doom--ediff-saved-wconf))
  )

;;;###autoload
(defun +jg-diff-unfold-h ()
  (when (-any #'symbol-value
              (-filter #'boundp
                       fold-modes))
    (evil-open-folds))
  (vimish-fold-unfold-all)
  )
