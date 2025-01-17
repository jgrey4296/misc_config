;;; state-line.el -*- lexical-binding: t; -*-


;;;###autoload
(defun +jg-ui-state-line-change ()
  (interactive)
  (hl-line-unhighlight)
  (setq-local hl-line-face (cond ((eq evil-state 'insert)  'jg-insert-line)
                           ((eq evil-state 'visual)  'jg-visual-line)
                           ((eq evil-state 'motion)  'jg-motion-line)
                           ((eq evil-state 'replace) 'jg-replace-line)
                           ((eq evil-state 'iedit)   'jg-iedit-line)
                           (t 'jg-normal-line)))
  (hl-line-highlight)
  )
