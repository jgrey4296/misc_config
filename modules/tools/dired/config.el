;;; tools/dired/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(defer-load! (jg-bindings-total jg-dired) "+bindings")

(use-package! dired
  :commands dired-jump
  :config
  (provide 'jg-dired)
  (spec-handling-add! evil-initial
                      '(image-dired-display-iamge-mode emacs)
                      )
  (if (not (executable-find "gls"))
      (setq dired-listing-switches (car dired-args)
            insert-directory-program "ls")
    )

  (setq dired-listing-switches (string-join dired-args " "))


  (put 'dired-find-alternate-file 'disabled nil)

  (defadvice! +dired--no-revert-in-virtual-buffers-a (&rest args)
    "Don't auto-revert in dired-virtual buffers (see `dired-virtual-revert')."
    :before-while #'dired-buffer-stale-p
    (not (eq revert-buffer-function #'dired-virtual-revert)))


  (add-hook! 'dired-mode-hook '+dired-disable-gnu-ls-flags-maybe-h)
  ;; (add-hook! 'dired-after-readin-hook nil)
  ;; (add-hook! 'dired-before-readin-ook nil)
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
  (setq fd-dired-program doom-projectile-fd-binary)
  (spec-handling-add! popup
                      '(fd-dired
                        ("^\\*F\\(?:d\\|ind\\)\\*$" :ignore t)
                        )
                      )
  )

(use-package! dired-aux
  :defer t
  )
