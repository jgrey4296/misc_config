;;; +extra.el -*- lexical-binding: t; no-byte-compile: t; -*-

(use-package! dired-rsync
  :after dired
  )

(use-package! dired-quick-sort
  :commands hydra-dired-quick-sort/body
  )

(use-package! fd-dired
  :when doom-projectile-fd-binary
  :defer t
  :init
  (global-set-key [remap find-dired] #'fd-dired)
  (setq fd-dired-program doom-projectile-fd-binary)
  (speckler-add! popup ()
    '(fd-dired
      ("^\\*F\\(?:d\\|ind\\)\\*$" :ignore t)
      )
    )
  )

(use-package! dired-aux
  :defer t
  )

(use-package! dired-imenu
  :after (dired imenu)
  )

;;; +extra.el ends here
