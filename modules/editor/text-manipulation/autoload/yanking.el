;;; yanking.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-text-yank-buffer-name ()
  (interactive)
  (message (kill-new (buffer-name)))
  )

;;;###autoload
(defun +jg-text-yank-selection-to-new-buffer ()
  (interactive)
  (let ((text (buffer-substring evil-visual-beginning evil-visual-end))
        (new-buf (get-buffer-create (read-string "New Buffer Name: "))))
    (with-current-buffer new-buf
      (insert text)
      (goto-char (point-min))
      )
    (display-buffer new-buf)
    (select-window (get-buffer-window new-buf))
    )
  )

;;;###autoload
(defun +jg-text-get-line ()
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position))
  )
