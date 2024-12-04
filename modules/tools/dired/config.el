;;; tools/dired/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(defer-load! (jg-bindings-total jg-dired) "+bindings")

(advice-add 'read-file-name-default :around #'+jg-dired-find-file-with-insert-plus-a)
(advice-add 'counsel-find-file :around #'+jg-dired-find-file-with-insert-plus-a)

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

  (advice-add 'dired-buffer-stale-p :before-while #'+dired--no-revert-in-virtual-buffers-a)

  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
  (add-hook! 'dired-before-readin-hook #'(lambda () (when (file-remote-p default-directory) (setq dired-actual-switches "-al"))))
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
        )
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

(use-package! dired-imenu
  :after (dired imenu)
  )
