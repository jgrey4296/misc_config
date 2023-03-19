;;; +derived-modes.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;-- scons
;;;###autodef
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

;;-- doit/doot
;;;###autodef
(define-derived-mode doot-mode python-mode "doot"
  ""
  (interactive)
  (setq-local major-mode 'doot-mode)
  (setq-local mode-name  "doot")
  (run-mode-hooks)
  )

(defun doot-open-toml ()
  (interactive)
  (when (and (s-matches? "dooter\\.py" (buffer-name))
             (f-exists? (f-join default-directory "doot.toml")))
    (split-window-right)
    (with-selected-window (selected-window)
      (find-file (f-join default-directory "doot.toml")))
    ))

(add-to-list 'auto-mode-alist '("dooter\\.py" . doit-mode))

;;-- end doit/doot
