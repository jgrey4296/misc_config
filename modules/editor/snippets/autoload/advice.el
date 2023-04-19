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
