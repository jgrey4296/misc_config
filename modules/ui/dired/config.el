;;; tools/dired/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+funcs")
(load! "+hooks")
(after! (jg-bindings-total jg-dired)
  (load! "+bindings")
  )

(use-package! dired
  :commands dired-jump
  :config
  (set-evil-initial-state! 'image-dired-display-image-mode 'emacs)

  (if (not (executable-find "gls"))
      (setq dired-listing-switches (car dired-args)
            insert-directory-program "ls")
    )

  (setq dired-listing-switches (string-join dired-args " "))

  (add-hook! 'dired-mode-hook '+dired-disable-gnu-ls-flags-maybe-h)

  (put 'dired-find-alternate-file 'disabled nil)

  (defadvice! +dired--no-revert-in-virtual-buffers-a (&rest args)
    "Don't auto-revert in dired-virtual buffers (see `dired-virtual-revert')."
    :before-while #'dired-buffer-stale-p
    (not (eq revert-buffer-function #'dired-virtual-revert)))

  (provide 'jg-dired)
  )

(use-package! dired-quick-sort
  :commands hydra-dired-quick-sort/body
  )

(use-package! dirvish
  :when (modulep! +dirvish)
  :defer t
  :init (after! dired (dirvish-override-dired-mode))
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dirvish-cache-dir (concat doom-cache-dir "dirvish/")
        dirvish-hide-details nil
        dirvish-attributes '(git-msg)
        dired-omit-files (concat dired-omit-files "\\|^\\..*$"))
  )


(use-package! dired-rsync
  :after dired
  )

(use-package! diredfl
  :hook (dired-mode . diredfl-mode)
  :config
  (set-face-attribute 'diredfl-flag-mark-line nil :background "blueviolet")
)

(use-package! dired-x
  :unless (modulep! +dirvish)
  :unless (modulep! +ranger)
  :hook (dired-mode . dired-omit-mode)
)

(use-package! fd-dired
  :when doom-projectile-fd-binary
  :defer t
  :init
  (global-set-key [remap find-dired] #'fd-dired)
  (set-popup-rule! "^\\*F\\(?:d\\|ind\\)\\*$" :ignore t)
  )

(use-package! dired-aux
  :defer t
  )
