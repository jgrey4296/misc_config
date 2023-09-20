;;; ui/doom/config.el -*- lexical-binding: t; -*-

;;;###package pos-tip
(setq pos-tip-internal-border-width 6
      pos-tip-border-width 1)


(use-package! doom-themes
  ;; improve integration w/ org-mode
  :hook (doom-load-theme . doom-themes-org-config)
  )

(use-package! solaire-mode
  :hook (doom-load-theme . solaire-global-mode)
  :hook (+popup-buffer-mode . turn-on-solaire-mode)
  )
