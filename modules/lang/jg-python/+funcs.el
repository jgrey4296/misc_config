;;; lang/jg-python/+funcs.el -*- lexical-binding: t; -*-
(defun +jg-personal-toggle-all-defs ()
    (interactive)
    ;; goto start of file
    (let* ((open-or-close 'evil-close-fold) ;;outline-hide-subtree)
           )
      (save-excursion
        (goto-char (point-min))
        (while (python-nav-forward-defun)
          (python-nav-beginning-of-defun)
          (if (not (looking-at-p "class"))
              (funcall open-or-close))
          (end-of-line)
          )
        )
      )
    )
(defun +jg-personal-close-class-defs ()
    (interactive )
    (save-excursion
      (goto-char (point-max))
      (while (python-nav-backward-defun)
        (if (looking-at-p "\s*def")
            (evil-close-fold))
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


(defun +jg-python-select-defun ()
  (interactive)
  (let ((start (progn (python-nav-beginning-of-defun)
                      (point)))
        (end (progn (python-nav-end-of-defun)
                    (point))))
    (evil-visual-make-region start end))
  )
(defun +jg-python-select-class ()
  (interactive)
  (let ((start (re-search-backward "^class")))
    (python-nav-end-of-defun)
    (evil-visual-make-region start (point))
    )
  )

