;;; +funcs.el -*- lexical-binding: t; -*-


;;;###autoload (autoload #'+jg-python-forward-defun "lang-weakly-typed/python/autoload/funcs.el" nil t)
(evil-define-motion +jg-python-forward-defun (count)
  " Custom Python movement, taking fold-blocks into account "
  :jump t
  :type exclusive
  (evil-signal-at-bob-or-eob count)
  (let ((fold-block-pos (point))
        (defun-pos (point)))
    (save-excursion
      (setq defun-pos (py-down-def-or-class)))
    (save-excursion
      (setq fold-block-pos (re-search-forward (code-shy-fold-block-gen :re t) defun-pos t)))
    (goto-char (apply 'min (mapcar #'(lambda (x) (if x x (point-max))) (list fold-block-pos defun-pos))))
    )
)

;;;###autoload
(defun +jg-python-make-test-file ()
  (interactive)
  (let* ((curr-file (buffer-file-name))
         (test-dir (f-join (f-parent curr-file) "__tests"))
         (test-file (format "test_%s" (f-filename curr-file)))
        )
    (cond ((f-exists? (f-join test-dir test-file))
           (message "Test File already exists"))
          ((f-exists? test-dir)
           (find-file (f-join test-dir test-file)))
          (t (mkdir test-dir)
             (find-file (f-join test-dir test-file)))
          )
    )
  )

;;;###autoload
(defun +jg-python-swap-ts-mode ()
  "Swap between python-mode and python-ts-mode"
  (interactive)
  (pcase major-mode
    ('python-mode (python-ts-mode))
    ('python-ts-mode (python-mode))
    (_ (user-error "Unknown Python mode"))
    )
  )


;;;###autoload
(defun +jg-python-get-editable-locs ()
  "Get the locations of editable packages in the current env,
to update MYPYPATH with
essentially:
uv pip list -e -q --format json | jq .[].editable_project_location
 "
  (with-temp-buffer
    (call-process "uv" nil t nil "pip" "list" "-e" "-q" "--format" "json")
    (call-process-region nil nil "jq" t t t ".[].editable_project_location")
    (replace-string "\"" "" nil (point-min) (point-max))
    (string-lines (buffer-string) t nil)
    )
  )
