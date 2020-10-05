;;; lang/python/+funcs.el -*- lexical-binding: t; -*-

(defun +jg-personal-toggle-all-defs ()
    (interactive)
    ;; goto start of file
    (let* ((open-or-close 'evil-close-fold) ;;outline-hide-subtree)
           )
      (save-excursion
        (goto-char (point-min))
        (while (python-nav-forward-defun)
          (beginning-of-line)
          (if (and (python-info-looking-at-beginning-of-defun)
                   (not (string-equal (current-word) "class")))
              (funcall open-or-close))
          (end-of-line)
          )
        )
      )
    )
(defun +jg-personal-close-class-defs ()
    (interactive )
    (save-excursion
      (let* ((current (point)))
        (python-nav-backward-defun)
        (while (and (not (+jg-personal-line-starts-with? "class "))
                    (not (equal current (point))))
          (evil-close-fold)
          (setq current (point))
          (python-nav-backward-defun)
          )
        )
      )
    (save-excursion
      (let* ((current (point)))
        (python-nav-forward-defun)
        (while (and (not (+jg-personal-line-starts-with? "class "))
                    (not (equal current (point))))
          (evil-close-fold)
          (setq current (point))
          (python-nav-forward-defun)
          )
        )
      )
    )
(defun +jg-personal-python-toggle-breakpoint ()
    "Modified version of spacemacs original
Add a break point, highlight it.
Customize python using PYTHONBREAKPOINT env variable
"
    (interactive)
    (let ((trace "breakpoint()")
          (line (thing-at-point 'line)))
      (if (and line (string-match trace line))
          (kill-whole-line)
        (progn
          (back-to-indentation)
          (insert trace)
          (insert "\n")
          (python-indent-line)))))
