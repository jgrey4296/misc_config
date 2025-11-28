;;; +dirvish.el -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package! dirvish
  :when (modulep! +dirvish)
  :defer t
  :init (after! dired (dirvish-override-dired-mode))
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dirvish-cache-dir (concat doom-cache-dir "dirvish/")
        dirvish-hide-details nil
        dirvish-attributes '(git-msg)
        )
  )

(use-package! dired-x
  :unless (modulep! +dirvish)
  :hook (dired-mode . dired-omit-mode)
  )

;;; +dirvish.el ends here
