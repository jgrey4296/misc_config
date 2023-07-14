;;; auto-hide.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-python-auto-hide ()
  " Add auto-hiding to python.
Hides imports in a vimish fold,
Add any sections commented with jg-python-fold-block-[start|end]-re
and closes classes and functions, re-opening only the first class "
  (when code-shy-minor-mode
    (message "Running Python Auto Hide")
    (save-excursion
      (beginning-of-buffer)
      ;; Fold Imports
      (message "Searching for import block")
      (evil-close-folds)
      (when (re-search-forward "^class " nil t)
        (let ((current-prefix-arg t))
          (evil-close-fold))
        )
      )
    )
  )

;;;###autoload
(defun +jg-python-close-all-defs ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (python-nav-forward-defun)
        (outline-hide-subtree)
        )
      )
    )

;;;###autoload
(defun +jg-python-close-class-defs ()
    (interactive)
    (save-excursion
      (end-of-line)
      (unless (not (re-search-backward "^class " nil t))
        (if (not current-prefix-arg)
            (progn
              (outline-hide-subtree)
              (outline-toggle-children))
          (outline-show-subtree)
          (forward-line)
          (while (and (python-nav-forward-defun)
                      (progn (beginning-of-line)
                             (not (looking-at-p "^class"))))
            (outline-toggle-children)
            (forward-line)
            )
          )
        )
      )
    )
