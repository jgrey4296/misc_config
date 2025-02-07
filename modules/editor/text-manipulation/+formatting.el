;; +formatting.el -*- lexical-binding: t; -*-

(add-to-list 'doom-debug-variables 'format-all-debug)
(when (modulep! +onsave)
  (add-hook 'after-change-major-mode-hook #'+format-enable-on-save-maybe-h))

(use-package! format-all
 :disabled t
 :init
  (advice-add 'format-all-buffer--from-hook :around #'+format--all-buffer-from-hook-a)
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
  :preface
  (remove-hook 'doom-first-buffer-hook #'ws-butler-global-mode)
  :config
  (ws-butler-global-mode -1)
  (pushnew! ws-butler-global-exempt-modes
            'special-mode
            'comint-mode
            'term-mode
            'eshell-mode
            'diff-mode)
  )

(use-package! adaptive-wrap
  :disabled
  )

(use-package! indent-tools)
