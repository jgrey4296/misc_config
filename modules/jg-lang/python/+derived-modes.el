;;; +derived-modes.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;-- scons
(define-derived-mode scons-mode python-mode "scons"
  ""
  (interactive)
  (setq-local major-mode 'scons-mode)
  (setq-local mode-name  "scons")
  (run-mode-hooks)
  )

(add-to-list 'auto-mode-alist '("SConscript" . scons-mode))
(add-to-list 'auto-mode-alist '("SConstruct" . scons-mode))
;;-- end scons

;;-- doit
(define-derived-mode doit-mode python-mode "doit"
  ""
  (interactive)
  (setq-local major-mode 'doit-mode)
  (setq-local mode-name  "doit")
  (run-mode-hooks)
  )

(add-to-list 'auto-mode-alist '("dodo.*\\.py" . doit-mode))

;;-- end doit
