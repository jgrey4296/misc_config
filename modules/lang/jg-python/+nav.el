;;; +nav.el -*- lexical-binding: t; -*-

;; Customisations of navigation functions

(evil-define-motion +jg-python-forward-defun (count)
  " Custom Python movement, taking fold-blocks into account "
  :jump t
  :type exclusive
  (evil-signal-at-bob-or-eob count)
  (let ((fold-block-pos (point))
        (defun-pos (point)))
    (save-excursion
      (setq defun-pos (python-nav-forward-defun)))
    (save-excursion
      (setq fold-block-pos (re-search-forward (+jg-fold-block-gen :re t) defun-pos t)))
    (goto-char (apply 'min (mapcar '(lambda (x) (if x x (point-max))) (list fold-block-pos defun-pos))))
    )
)
