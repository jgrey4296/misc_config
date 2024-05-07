;; import-view.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-python--import-view ()
  "Open a popup indirect buffer of just the imports in the current python file"
  (interactive)
  (when (get-buffer "*imports*")
    (kill-buffer "*imports*"))
  (let ((imports (clone-indirect-buffer "*imports*" t))
        start end
        )
    (with-current-buffer imports
      (goto-char (point-min))
      (widen)
      (re-search-forward "import")
      (setq start (line-beginning-position))
      (forward-line)
      (re-search-forward (rx (or "end 1st party imports" "end imports")))
      (forward-line)
      (while (looking-at "^\\($\\|.*import\\)")
        (forward-line)
        )
      (forward-line -1)
      (setq end (line-end-position))
      (narrow-to-region start end)
      (goto-char (point-max))
      )
    )
  )
