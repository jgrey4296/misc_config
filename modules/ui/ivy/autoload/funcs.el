;; funcs.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-ivy-popup-messages (&optional arg)
  "Popup (and maybe clear the messages, or warnings, buffer"
  (interactive "p")
  (pcase arg
    (1  ;; messages
     (+jg-popup-ivy-open messages-buffer-name)
     (with-selected-window (get-buffer-window messages-buffer-name)
       (goto-char (point-max))
       (recenter -1)))
    (4  ;; clear messages
     (+jg-popup-ivy-open messages-buffer-name)
     (with-selected-window (get-buffer-window messages-buffer-name)
       (+jg-text-clear-buffer)
       (goto-char (point-max))
       (recenter -1)))
    ((and x (guard (get-buffer "*Warnings*")))  ;; clear warnings
     (+jg-popup-ivy-open  "*Warnings*")
     (with-selected-window (get-buffer-window "*Warnings*")
       (when (cl-evenp x) (+jg-text-clear-buffer))
       (goto-char (point-max))
       (recenter -1)))
    (x (message "No Warnings buffer exists"))
    )
  )

;;;###autoload
(defun +jg-ivy-file-predicate (x)
  (and (s-matches? jg-ivy-file-regexp x)
       (not (s-matches? jg-ivy-file-reject-regexp x)))
  )
