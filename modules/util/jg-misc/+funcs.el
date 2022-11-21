;;; util/jg-misc/+funcs.el -*- lexical-binding: t; -*-

;;-- misc
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
(defun +jg-misc-flatten (lst)
  " Utility to flatten a list "
  (letrec ((internal (lambda (x)
                       (cond
                        ((null x) nil)
                        ((atom x) (list x))
                        (t
                         (append (funcall internal (car x)) (funcall internal (cdr x))))))))
    (progn
      (cl-assert (listp lst))
      (funcall internal lst))))
(defun +jg-misc-line-starts-with? (text)
  (s-starts-with? text (s-trim-left (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position))))
  )
(defun +jg-misc-modify-line-end-display-table ()
  (interactive)
  " from https://stackoverflow.com/questions/8370778/ "
  ;; Modify the display table for whitespace, so lines which
  ;; truncate are not signaled with a $
  (set-display-table-slot standard-display-table 0 ?\ )
  )

(defun +jg-misc-sync-movements ()
  ;; TODO
  ;; Get current windows
  ;; add advice to evil line move

  )

;;-- end misc
