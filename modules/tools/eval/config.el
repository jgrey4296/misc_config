;;; tools/eval/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(local-load! "+spec-defs")

(defer-load! jg-bindings-total "+bindings")

(defer-load! jg-evil-ex-bindings "+evil-ex")

(advice-add 'quickrun--outputter-replace-region :override #'+eval--quickrun-fix-evil-visual-region-a)
(advice-add 'quickrun :before #'+eval--quickrun-auto-close-a)
(advice-add 'quickrun-region :before #'+eval--quickrun-auto-close-a)
(advice-add 'quickrun--make-sentinel :filter-return #'+eval--show-output-in-overlay-a)
(advice-add 'quickrun--pop-to-buffer :override #'+eval--inhibit-quickrun-popup-a)
(advice-add 'quickrun--make-sentinel :filter-return #'+eval--show-output-in-overlay-a)
(advice-add 'quickrun--pop-to-buffer :override #'+eval--inhibit-quickrun-popup-a)
(advice-add #'quickrun--recenter :override #'ignore)

(use-package! quickrun
  :config
  (setq quickrun-focus-p nil)

  (add-hook! 'quickrun-after-run-hook
             #'+eval-quickrun-shrink-window-h
             #'+eval-quickrun-scroll-to-bof-h
             )
  )

(use-package! eros
  :commands eros-mode
  :hook (emacs-lisp-mode . eros-mode)
  )

(use-package! compile
  :defer t
  :config
  (add-hook 'compilation-mode-hook   #'hl-line-mode)
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer)

  (autoload 'comint-truncate-buffer "comint" nil t)
  )
