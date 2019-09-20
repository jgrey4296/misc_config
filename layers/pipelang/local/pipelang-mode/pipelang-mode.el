




(define-derived-mode pipelang-mode fundamental-mode
  "Pipelang Mode"
  "Major Mode for specifying pipeline architectures"
  (interactive)
  (kill-all-local-variables)
  ;; (use-local-map pipe-mode-map)
  ;; (set (make-local-variable 'font-lock-defaults) '(pipe-font-lock-keywords))
  ;; (set (make-local-variable 'indent-line-function) 'pipe-indent-line)
  (set (make-local-variable 'comment-style) '(plain))
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-use-syntax) t)
  ;; (set-syntax-table pipe-mode-syntax-table)
  (setq major-mode 'pipelang-mode)
  (setq mode-name "PIPELANG")
  (run-mode-hooks)
  )

(provide 'pipelang-mode)
