;;; +derived-modes.el -*- lexical-binding: t; -*-

;;;###autoload
(define-derived-mode license-mode text-mode "license"
  ""
  (interactive)
  (setq-local major-mode 'license-mode)
  (setq-local mode-name  "license")
  (run-mode-hooks)
  )
