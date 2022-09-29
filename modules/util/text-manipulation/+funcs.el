;;; util/text/+funcs.el -*- lexical-binding: t; -*-
(defun +jg-text-get-line ()
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
  (yas-expand-snippet (yas-lookup-snippet jg-text-debug-snippet-name) (point))
  )


(defun +jg-text-combine-columns (textlst)
  " particularly for hydra docs "
  (let* ((cols-as-lines (mapcar (lambda (x)
                                 (split-string x "\n"))
                               textlst))
         (longest-in-col (mapcar (-partial 'apply 'max)
                                 (mapcar (-partial 'mapcar 'length)
                                         cols-as-lines)))
         (padded (cl-loop for col in (-zip-lists longest-in-col cols-as-lines)
                          collect
                          (mapcar (-partial 's-pad-right (car col) " ") (cadr col))
                          ))
         (line-counts (mapcar #'length cols-as-lines))
        )
    (message "Got %s columns of %s lines of lengths %s"
             (length cols-as-lines) line-counts longest-in-col)
    (concat "\n" (string-join (cl-loop for cols in (apply '-zip-fill " " (make-list (length cols-as-lines) " ") padded)
                          collect
                          (string-join cols " | ")
                          )
                 "\n"))
    )
  )
