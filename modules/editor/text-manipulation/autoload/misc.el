;;; util/text/+funcs.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-text-insert-lparen ()
  " utility to insert a (  "
  (interactive)
  (insert "(")
  )

;;;###autoload
(defun +jg-text-insert-rparen ()
  " utility to insert a ) "
  (interactive)
  (insert ")")
  )

;;;###autoload
(defun +jg-misc-get-modes ()
  (let (major minor)
    ;; Modes in auto mode alist:
    (cl-loop for mode in (mapcar 'cdr auto-mode-alist)
          do
          (unless (consp mode)
            (cl-pushnew mode major)))

    (cl-loop for mode in (mapcar 'cdr auto-minor-mode-alist)
          do
          (unless (consp mode)
            (cl-pushnew mode minor)))

    ;; modes from packages:
    (cl-loop for pkg in (mapcar 'car (doom-package-list))
          do
          (cond ((string-match "-minor-mode$" (symbol-name pkg))
                 (cl-pushnew pkg minor))
                ((fboundp (intern (format "%s-minor-mode" pkg)))
                 (cl-pushnew (intern (format "%s-minor-mode" pkg)) minor))
                ((string-match "-mode$"  (symbol-name pkg))
                 (cl-pushnew pkg major))
                ((fboundp (intern (format "%s-mode" pkg)))
                 (cl-pushnew (intern (format "%s-mode" pkg)) major))
                (t nil)
                )
          )

    (list major minor)
    )
  )

;;;###autoload
(defun +jg-text-join-line-bol (beg end)
  " When joining lines, don't lose sight of the lhs of the buffer "
  (beginning-of-line)
  )

;;;###autoload
(defun +default-open-doc-comments-block (&rest _ignored)
  ;; Expand C-style comment blocks.
  (save-excursion
    (newline)
    (indent-according-to-mode))
  )

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

;;;###autoload
(defun +jg-text-insert-random-word ()
  (interactive)
  (insert (shell-command-to-string "randname"))
  )


;;;###autoload
(advice-add 'evil-join :after #'+jg-text-join-line-bol)
