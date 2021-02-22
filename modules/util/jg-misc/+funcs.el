;;; util/jg-misc/+funcs.el -*- lexical-binding: t; -*-

(defun +get-line ()
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position))
  )

(defun +jg-misc-uniquify (L R)
  (interactive "r")
  (save-excursion
    (goto-char L)
    (let ((R-mark (set-marker (make-marker) R))
          (current (+get-line))
          (kill-whole-line t))
      (forward-line 1)
      (while (<= (point) R-mark)
        ;; compare
        (if (s-equals? current (+get-line))
            (kill-line)
          (progn (setq current (+get-line))
                 (forward-line 1))
          )
        )
      )
    )
  )

(defun +jg-misc-undo-tree ()
  (interactive)
  (if (not undo-tree-mode)
      (undo-tree-mode))
   (undo-tree-visualize)

  )
