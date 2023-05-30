;;; +funcs.el -*- lexical-binding: t; -*-


;;;###autoload (autoload #'+jg-python-forward-defun "funcs" nil t)
(evil-define-motion +jg-python-forward-defun (count)
  " Custom Python movement, taking fold-blocks into account "
  :jump t
  :type exclusive
  (evil-signal-at-bob-or-eob count)
  (let ((fold-block-pos (point))
        (defun-pos (point)))
    (save-excursion
      (setq defun-pos (py-down-def-or-class)))
    (save-excursion
      (setq fold-block-pos (re-search-forward (autohide-minor-mode-fold-block-gen :re t) defun-pos t)))
    (goto-char (apply 'min (mapcar #'(lambda (x) (if x x (point-max))) (list fold-block-pos defun-pos))))
    )
)
