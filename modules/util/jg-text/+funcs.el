;;; util/text/+funcs.el -*- lexical-binding: t; -*-
(defun +jg-get-line ()
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position))
  )

(defun +jg-text-strip-spaces (str)
  "Utility to replace spaces with underscores in a string.
Used to guard inputs in tag strings"
  (s-replace " " "_" (string-trim str))
  )

(defun +jg-text-regex-reminder ()
  (interactive)
  (with-temp-buffer-window "*Regex Char Class Reminder*" 'display-buffer-pop-up-window
                           nil
    (princ (yas--template-content (yas-lookup-snippet "Char Classes" 'fundamental-mode)))
    )
  nil
  )

(defun +jg-text-remove-leading-whitespace ()
  (interactive)
  (let ((start (if (evil-visual-state-p) evil-visual-beginning (line-beginning-position)))
        (end (if (evil-visual-state-p) evil-visual-end (line-end-position))))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (beginning-of-line-text)
        (kill-region (line-beginning-position) (point))
        (forward-line)
        )
      )
    )
  )

(defun +jg-text-split-on-leading-char (char-no dist)
  (interactive "nChar Index: \nnDistance: \n")
  (message "Char: %s, Dist: %s" char-no dist)
  (goto-char (point-min))
  (let ((get-line (lambda () (downcase (buffer-substring-no-properties (line-beginning-position)
                                                        (min (point-max) (+ (line-beginning-position) char-no))))))
        last-line curr-line)
    (while (< (point) (point-max))
      (setq last-line (funcall get-line))
      (forward-line)
      (setq curr-line (funcall get-line))
      (if (< dist (string-distance last-line curr-line))
          (insert "\n")
        )
      )
    )
  )

(defun +jg-text-yank-buffer-name ()
  (interactive)
  (message (kill-new (buffer-name)))
  )
