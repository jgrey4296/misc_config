;;; +advice.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jg-pasp-generate-args (fun encoding &optional instance)
  (let ((original pasp-clingo-options)
        found-args
        pasp-clingo-options
        )
    (with-current-buffer (current-buffer)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^% pasp-arg:\\(.+\\)$" nil t)
          (push (string-trim (match-string 1)) found-args)
          )
        )
      )
    (setq pasp-clingo-options (string-join (cons original found-args) " "))
    (apply fun (list encoding instance))
    )
  )

;;;###autoload
(defun +jg-pasp-run-clingo (encoding &optional instance)
  (when (get-buffer pasp-results-buffer-name)
    (kill-buffer pasp-results-buffer-name))
  (let ((test-command-to-run (pasp-generate-command encoding instance))
        (compilation-buffer-name-function (lambda (_) "" pasp-results-buffer-name)))
    (compile test-command-to-run 'pasp-compilation-mode)
    )
  )

;;;###autoload
(defun +jg-pasp-compilation-filter ()
  (ansi-color-apply-on-region compilation-filter-start (point-max))
  (save-excursion
    (while (re-search-forward "^[\\[[0-9]+[a-z]" nil t)
      (replace-match "")
      )
    ;; (goto-char compilation-filter-start)
    (goto-char (point-max))
    (if (not (process-live-p (get-buffer-process (current-buffer))))
        (let ((content (buffer-string)))
            (with-current-buffer (get-buffer-create "*result*")
              (erase-buffer)
              (insert content)
              (goto-char (point-min))
              )
            )
      ;; (insert "\nFinished")
      ;; (insert "\nRunning")
      )
    )
  )

;;;###autoload
(advice-add 'pasp-generate-command :around #'+jg-pasp-generate-args)

;;;###autoload
(advice-add 'pasp-run-clingo :override #'+jg-pasp-run-clingo)

;;;###autoload
(advice-add 'pasp-compilation-filter :override #')
