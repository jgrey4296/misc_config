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

(defun +jg-text-split-on-leading-char (char-no width dist)
  " Loop through the buffer, splitting lines if the substring has a greater levenstein distance from the previous line "
  (interactive "nChar Index: \nnWidth: \nnDistance: \n")
  (message "Char: %s, Dist: %s" char-no dist)
  (goto-char (point-min))
  (let ((get-line (lambda () (let ((line (s-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
                               (if (string-empty-p line)
                                   ""
                                 (substring line char-no (+ char-no width))))))
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

(defun +jg-text-toggle-auto-hide ()
  (interactive)
  (setq jg-text-auto-hide-toggle (not jg-text-auto-hide-toggle))
  (message "Auto Hide: %s" jg-text-auto-hide-toggle))


(defun +jg-text-clear-buffer ()
  " Utility to clear a buffer
    from https://stackoverflow.com/questions/24565068/ "
  (interactive)
  (let ((inhibit-read-only t)) (erase-buffer))
  )
(defun +jg-text-insert-lparen ()
  " utility to insert a (  "
  (interactive)
  (insert "(")
  )
(defun +jg-text-insert-rparen ()
  " utility to insert a ) "
  (interactive)
  (insert ")")
  )

(defun +jg-text-insert-debug ()
  (interactive)
  (yas-expand-snippet (yas-lookup-snippet jg-binding-debug-snippet-name) (point))
  )
