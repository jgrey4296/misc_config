;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: May 04, 2023
;; Modified: May 04, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.2))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:


(local-load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(use-package! shell
  :config
  (add-hook 'shell-mode-hook #'doom-mark-buffer-as-real-h)
  (add-hook 'shell-mode-hook #'hide-mode-line-mode)

  )

(use-package! term
  :config
  (add-hook 'term-mode-hook #'doom-mark-buffer-as-real-h)
  (add-hook 'term-mode-hook #'hide-mode-line-mode)
  (setq-hook! 'term-mode-hook hscroll-margin 0)
  )

(use-package! vterm
  :when (featurep 'dynamic-modules)
  :commands vterm-mode
  :hook (vterm-mode . doom-mark-buffer-as-real-h)
  :hook (vterm-mode . hide-mode-line-mode) ; modeline serves no purpose in vterm
  :preface
  ;; HACK Because vterm clusmily forces vterm-module.so's compilation on us when
  ;;      the package is loaded, this is necessary to prevent it when
  ;;      byte-compiling this file (`use-package' blocks eagerly loads packages
  ;;      when compiled).
  (when noninteractive
    (advice-add 'vterm-module-compile :override #'ignore)
    (provide 'vterm-module))

  :config

  ;; 5000 lines of scrollback, instead of 1000

  (setq-hook! 'vterm-mode-hook
    ;; Don't prompt about dying processes when killing vterm
    confirm-kill-processes nil
    ;; Prevent premature horizontal scrolling
    hscroll-margin 0)

  )

(use-package! comint
  :config
  (add-hook! 'comint-mode-hook (defun jg-remove-comint-capf ()
                                 (remove-hook 'completion-at-point-functions 'comint-completion-at-point t)
                                 (add-hook 'completion-at-point-functions 'comint-dynamic-complete-filename nil t)
                                 ))
  )

(use-package! shell-pop)

(use-package! exec-path-from-shell
  :disabled
  )
;;; config.el ends here
