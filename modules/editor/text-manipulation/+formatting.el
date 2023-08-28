;; +formatting.el -*- lexical-binding: t; -*-

(add-to-list 'doom-debug-variables 'format-all-debug)
(when (modulep! +onsave)
  (add-hook 'after-change-major-mode-hook #'+format-enable-on-save-maybe-h))

(use-package! format-all
 :disabled t
  )

(use-package! dtrt-indent
  ;; Automatic detection of indent settings
  :unless noninteractive
  ;; I'm not using `global-dtrt-indent-mode' because it has hard-coded and rigid
  ;; major mode checks, so I implement it in `doom-detect-indentation-h'.
  :hook ((change-major-mode-after-body read-only-mode) . doom-detect-indentation-h)
  :config

  ;; Enable dtrt-indent even in smie modes so that it can update `tab-width',
  ;; `standard-indent' and `evil-shift-width' there as well.
  (setq dtrt-indent-run-after-smie t)
  ;; Reduced from the default of 5000 for slightly faster analysis
  (setq dtrt-indent-max-lines 2000)

  ;; always keep tab-width up-to-date
  (push '(t tab-width) dtrt-indent-hook-generic-mapping-list)

  (defvar dtrt-indent-run-after-smie)
  )

(use-package! ws-butler
  ;; a less intrusive `delete-trailing-whitespaces' on save
  :hook (doom-first-buffer . ws-butler-global-mode)
  )

(use-package! adaptive-wrap
  :disabled
  )
