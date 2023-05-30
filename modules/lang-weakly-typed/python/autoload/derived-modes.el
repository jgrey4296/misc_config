;;; +derived-modes.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;-- scons
;;;###autoload
(define-derived-mode scons-mode python-mode "scons"
  ""
  (interactive)
  (setq-local major-mode 'scons-mode)
  (setq-local mode-name  "scons")
  (run-mode-hooks)
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("SConscript" . scons-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("SConstruct" . scons-mode))
;;-- end scons

;;-- doit/doot
;;;###autoload
(define-derived-mode doot-mode python-mode "doot"
  ""
  (interactive)
  (setq-local major-mode 'doot-mode)
  (setq-local mode-name  "doot")
  (run-mode-hooks)
  )

;;;###autoload
(defun doot-open-toml ()
  (interactive)
  (when (and (s-matches? "dooter\\.py" (buffer-name))
             (f-exists? (f-join default-directory "doot.toml")))
    (split-window-right)
    (with-selected-window (selected-window)
      (find-file (f-join default-directory "doot.toml")))
    ))

;;;###autoload
(add-to-list 'auto-mode-alist '("dooter\\.py" . doit-mode))

;;-- end doit/doot
