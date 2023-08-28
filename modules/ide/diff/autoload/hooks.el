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
