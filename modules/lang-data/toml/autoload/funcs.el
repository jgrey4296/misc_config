;;; +funcs.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-toml-related-files-fn (path)
  " Provide projectile with various :kinds of related file "
  (let* ((root (projectile-project-root))
         (fbase   (f-base path))
         (fname   (f-filename path))
         (fparent (f-parent path))

         (doot-toml  (f-join root "doot.toml"))
         (dooter     (f-join root "dooter.py"))
         (project    (f-join root "doot.toml"))

         (is-test      (s-matches? "^test_" fname))
         (is-dooter    (s-matches? "dooter.py" fname))
         (is-doot-toml (s-matches? "doot.toml" fname))
         )
    (append (when is-dooter (list :config doot-toml))
            (when is-doot-toml (list :tasks dooter))
            (list :project (f-join root "dooter.py"))
            (list :log (f-join root "log.doot"))
            )
    )
  )

;;;###autoload
(defun +jg-toml-cleanup-ensure-newline-before-table ()
  (while (re-search-forward "\\(\n\\)\\(#.+?\n\\)*\\(\\[.+?\\]\\)" nil t)
    (goto-char (match-end 1))
    (insert "\n")
    (goto-char (match-end 0))
    )
  )

;;;###autoload
(defun jg-toml-outline-level ()
  " Toml levels:
1: ^[?[.+?]]?
1: ^# ----
2: ^.+? = [$
"
  (cond ((looking-at-p (rx (+? any) "=" (+ space) "[" line-end))
         2)
        ((looking-at-p (rx line-start "["))
         1)
        ((looking-at-p (rx line-start "# " (1+ "-") line-end))
         0)
        (t 3)
        )
  )
