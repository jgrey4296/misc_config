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

;;-- end cleaning

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


;;-- formatting
(defun +jg-text-combine-columns (textlst)
  (let* ((rows-of-cols (mapcar (lambda (x)
                                 (mapcar 's-trim (split-string x "\n")))
                               textlst))
         (longest-in-col (mapcar (-partial 'apply 'max)
                                 (mapcar (-partial 'mapcar 'length)
                                         rows-of-cols)))
         (padded (cl-loop for col in (-zip-lists longest-in-col rows-of-cols)
                          collect
                          (mapcar (-partial 's-pad-right (car col) " ") (cadr col))
                          ))
         (line-counts (mapcar #'length rows-of-cols))
        )
    (message "Got %s columns of %s lines of lengths %s"
             (length rows-of-cols) line-counts longest-in-col)
    (concat "\n" (string-join (cl-loop for cols in (apply '-zip-fill " " (make-list (length rows-of-cols) " ") padded)
                          collect
                          (string-join cols " | ")
                          )
                 "\n"))
    )
  )

(defun +jg-text-format-hydra-columns (textlst)
  " particularly for hydra docs "

  (let* (;; Splt columns into lists of rows
         (rows-of-cols (mapcar (lambda (x)
                                 (mapcar 's-trim (split-string x "\n")))
                               textlst))
         ;; Get the longest row
         (longest-in-col (funcall (-compose (-partial 'mapcar
                                                      (-partial 'apply 'max))
                                            (-partial 'mapcar
                                                      (-partial 'mapcar 'length)))
                                  rows-of-cols))
         ;; Pad all columns in each row
         (padded (cl-loop for col in (-zip-lists longest-in-col rows-of-cols)
                          collect
                          (mapcar (-partial 's-pad-right (car col) "^") (cadr col))
                          ))
         ;; Count the number of columns in each column
         (row-counts (mapcar #'length rows-of-cols))
         ;; make a tuple of the row count with the padded columns
         (tuple-pad (apply '-zip-fill " " (make-list (length rows-of-cols) " ") padded))
        )
    ;; Join all column's for each row
    ;; Then join all rows
    (concat "\n" (string-join
                  (cl-loop for cols in tuple-pad
                           collect
                           (string-join cols " | ")
                          )
                  "\n"))
    )
  )

;;-- end formatting
