;;; advice.el -*- lexical-binding: t; -*-

;;; Advice

;;;###autoload
(defun +snippets-expand-on-region-a (fn &optional no-condition)
  "Fix off-by-one when expanding snippets on an evil visual region.

Also strips whitespace out of selection. Also switches to insert mode. If
`evil-local-mode' isn't enabled, or we're not in visual mode, run FN as is."
  (if (not (and (bound-and-true-p evil-local-mode)
                (evil-visual-state-p)))
      (funcall fn no-condition)
    ;; Trim whitespace in selected region, so as not to introduce extra
    ;; whitespace into `yas-selected-text'.
    (evil-visual-select (save-excursion
                          (goto-char evil-visual-beginning)
                          (skip-chars-forward " \t")
                          (point))
                        (save-excursion
                          (goto-char evil-visual-end)
                          (skip-chars-backward " \t")
                          (point))
                        'inclusive)
    (letf! ((defun region-beginning () evil-visual-beginning)
            (defun region-end () evil-visual-end))
      (funcall fn no-condition)))
  (when (and (bound-and-true-p evil-local-mode)
             (not (or (evil-emacs-state-p)
                      (evil-insert-state-p)))
             (yas-active-snippets))
    (evil-insert-state +1)))

;;;###autoload
(defun +jg-snippets-read-table ()
  (let ((tables (hash-table-keys yas--tables)))
    (intern-soft (ivy-read "Snippet Table: " tables))
    )
  )

;;;###autoload
(advice-add 'yas--read-table :override #'+jg-snippets-read-table)

;;;###autoload
(defun +jg-snippets-doom-nullify ()
  nil
  )

;;;###autoload
(advice-add 'doom-snippets-initialize #'+jg-snippets-doom-nullify)

(defun +jg-snippets--remove-duplicates-a (templates)
  ;; REVIEW Fix #2639: For some reason `yas--all-templates' returns duplicates
  ;;        of some templates. Until I figure out the real cause this fixes it.
    (cl-delete-duplicates templates :test #'equal))

;;;###autoload
(advice-add 'yas--all-templates :filter-return #'+jg-snippets-remove-duplicates-a)

;;;###autoload
(defun +snippets--inhibit-yas-global-mode-a (fn &rest args)
    "auto-yasnippet enables `yas-global-mode'. This is obnoxious for folks like
us who use yas-minor-mode and enable yasnippet more selectively. This advice
swaps `yas-global-mode' with `yas-minor-mode'."

    (letf! ((#'yas-global-mode #'yas-minor-mode)
            (yas-global-mode yas-minor-mode))
      (apply fn args)))

;;;###autoload
(advice-add 'aya-expand :around #'+snippets--inhibit-yas-global-mode-a)

;;;###autoload
(advice-add 'aya-open-line :around #'+snippets--inhibit-yas-global-mode-a)
