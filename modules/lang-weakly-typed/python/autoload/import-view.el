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
      (forward-line -1)
      (setq start (line-beginning-position))
      (re-search-forward "end imports")
      (re-search-forward "import")
      (forward-line)
      (while (looking-at "^\\($\\|.*import\\)")
        (forward-line)
        )
      (forward-line -1)
      (setq end (line-end-position))
      (narrow-to-region start end)
      )
    )
  )

;;;###autoload
(defun +jg-python--add-import ()
  "TODO"
  (interactive)
  )
