;;; main/jg-personal/+ibuffer-funcs.el -*- lexical-binding: t; -*-

(defun +jg-personal-setup-ibuffer ()
  (interactive)
  (ibuffer-switch-to-saved-filter-groups "my-default")
  (ibuffer-switch-to-saved-filters "anti-helm-and-magit")
  )
