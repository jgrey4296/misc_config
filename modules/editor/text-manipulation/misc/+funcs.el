;;; util/text/+funcs.el -*- lexical-binding: t; -*-

;;-- cleaning

(defun +jg-text-strip-spaces (str)
  "Utility to replace spaces with underscores in a string.
Used to guard inputs in tag strings"
  (s-replace " " "_" (string-trim str))
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

(defun +jg-text-cleanup-whitespace ()
  " compact multiple newlines into just one "
  (while (re-search-forward "\n\n\\(\n+\\)" nil t)
    (let ((matched (length (match-string 1))))
      (replace-match "" nil nil nil 1)
      )
    )
  )

;;-- end cleaning

;;-- utils

(defun +jg-text-yank-buffer-name ()
  (interactive)
  (message (kill-new (buffer-name)))
  )

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

(defun +jg-text-clear-buffer ()
  " Utility to clear a buffer
    from https://stackoverflow.com/questions/24565068/ "
  (interactive)
  (let ((inhibit-read-only t)) (erase-buffer))
  )

(defun +jg-text-regex-reminder ()
  (interactive)
  (with-temp-buffer-window "*Regex Char Class Reminder*" 'display-buffer-pop-up-window
                           nil
    (princ (yas--template-content (yas-lookup-snippet "Char Classes" 'fundamental-mode)))
    )
  nil
  )

(defun +jg-text-get-line ()
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position))
  )
;;-- end utils

;;-- util inserts

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
  (yas-expand-snippet (yas-lookup-snippet jg-text-debug-snippet-name) (point))
  )

;;-- end util inserts

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

(defun +jg-text-run-whitespace-cleanup ()
  "Operator to run cleaning hooks"
  (interactive)
  (let ((inhibit-read-only t))
    (cl-loop for hook in jg-text-whitespace-clean-hook
             when (functionp hook)
             do
             (save-excursion
               (goto-char (point-min))
               (apply hook nil)
               )
             )
  )
)
