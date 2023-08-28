;;; evil-ex.el -*- lexical-binding: t; -*-


;;;###autoload (autoload '+evil:reverse-lines "editor/text-manipulation/autoload/evil-ex" nil t)
(evil-define-command +evil:reverse-lines (beg end)
  "Reverse lines between BEG and END."
  (interactive "<r>")
  (reverse-region beg end))


;;;###autoload (autoload '+jg-text-manipulation-ex-expand-selection "editor/text-manipulation/autoload/evil-ex" nil t)
(evil-define-command +jg-text-manipulation-ex-expand-selection (&optional pattern case wholeline)
  "Ex function to create a visual selection by regexp match

PATTERN is a vim-style regexp. FLAGS is an optional string of characters. "
  (interactive "<p/>")
  (let ((start (point))
        (beg (line-beginning-position))
        (end (line-beginning-position))
        (regexp (when (car-safe pattern) (format "\\(^$\\)\\|\\(^.*?\\(%s\\)\\)" (car-safe pattern))))
        )
    (when regexp
      (save-excursion
        (evil-beginning-of-line)
        ;; Expand up
        (while (and (looking-at regexp) (not (bobp)))
          (unless (looking-at "^$") (setq beg (line-beginning-position)))
          (evil-previous-line)
          (evil-beginning-of-line))

        (goto-char end)
        (goto-char (line-beginning-position 2))

        ;; Expand down
        (while (and (looking-at regexp) (not (eobp)))
          (unless (looking-at "^$") (setq end (line-beginning-position)))
          (evil-next-line)
          (evil-beginning-of-line))

        )
      (unless (and (eq (line-beginning-position) beg)
                   (eq (line-beginning-position 1) end))
        (evil-visual-make-selection beg end 'line)
        (add-hook 'post-command-hook #'+jg-text-manipulation-ensure-visual-selection-h)
        )
      )
    )
  )


(defun +jg-text-manipulation-ensure-visual-selection-h ()
  " Auto-removing post-command hook to ensure visual selection in evil-ex"
  (evil-visual-restore)
  (remove-hook 'post-command-hook #'+jg-text-manipulation-ensure-visual-selection-h)
  )
