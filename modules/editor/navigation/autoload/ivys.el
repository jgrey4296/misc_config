;;; ivys.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-navigation-marker-delete-action (cand)
  ;; (evil-delete-marks )
  (string-match "\\[\\(.\\)\\]" cand)
  (let ((marker (match-string 1 cand))
        )
    (message "Got: %s : %s" cand marker)
    (evil-delete-marks marker)
    )
  )
