;;; +derived-modes.el -*- lexical-binding: t; -*-

;;-- mode derivation
(define-derived-mode license-mode text-mode "license"
  ""
  (interactive)
  (setq-local major-mode 'license-mode)
  (setq-local mode-name  "license")
  (run-mode-hooks)
  )

(add-to-list 'auto-mode-alist '("LICENSE" . license-mode))
;;-- end mode derivation
