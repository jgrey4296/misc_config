;;; config/ui/popup/+funcs.el -*- lexical-binding: t; -*-

(defun +jg-ui-popup-add-rules (sym rules &optional override)
  " sym is a symbol to avoid adding duplicate rulesets

  Expects a list of form:
  '((PATTERN :opt val :opt val) (PATTERN :opt val :opt val))
  "
  (cl-assert (hash-table-p jg-popup-display-rules))
  (if (and (gethash sym jg-popup-display-rules) (not override))
      (message "Popup Ruleset %s already exists" sym)
    (puthash sym (cl-loop for (head . body) in rules
                          for priority = (* -1 (or (plist-get body :priority) 0))
                          collect (cons priority (+popup-make-rule head body)))
             jg-popup-display-rules)
    )
  )

(defun +jg-ui-popup-activate-rules (&optional force)
  (interactive "P")
  (when (or force (not jg-popup-display-flattened))
    (message "Reconstructing popup rules: %s" (hash-table-keys jg-popup-display-rules))
    (setq jg-popup-display-flattened
          (-concat (mapcar #'cdr (sort
                                  (copy-sequence
                                   (-flatten-n 1 (hash-table-values jg-popup-display-rules)))
                                  #'(lambda (x y) (< (car x) (car y))))
                           )
                   '(("*jg-customised*" (+popup-buffer)))
                   )
          )
    )
  (when jg-popup-display-flattened
    (message "Reapplying popup rules")
    (setq +popup--display-buffer-alist nil
          display-buffer-alist jg-popup-display-flattened)
    )
  )

(defun +jg-ui-popup-reapply-rules ()
  (interactive)
  (+jg-ui-popup-activate-rules t)
  )

(define-advice set-popup-rules! (:after (&rest args)
                                 +jg-popup-advice)
  (+jg-ui-popup-activate-rules))

(define-advice set-popup-rule! (:after (&rest args)
                                 +jg-popup-advice2)
  (+jg-ui-popup-activate-rules))
