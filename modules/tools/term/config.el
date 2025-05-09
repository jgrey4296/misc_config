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
  (add-hook 'shell-mode-hook #'shell-completion-vars 90)
  (add-hook 'shell-mode-hook #'librarian-insert-minor-mode)
  )

(use-package! term
  :defer t
  :config
  (add-hook 'term-mode-hook #'doom-mark-buffer-as-real-h)
  (add-hook 'term-mode-hook #'hide-mode-line-mode)
  (setq-hook! 'term-mode-hook hscroll-margin 0)
  )

(use-package! vterm
  :when (featurep 'dynamic-modules)
  :defer t
  :hook (vterm-mode . doom-mark-buffer-as-real-h)
  :hook (vterm-mode . hide-mode-line-mode)
  :preface
  ;; HACK Because vterm clusmily forces vterm-module.so's compilation on us when
  ;;      the package is loaded, this is necessary to prevent it when
  ;;      byte-compiling this file (`use-package' blocks eagerly loads packages
  ;;      when compiled).
  (when noninteractive
    (advice-add 'vterm-module-compile :override #'ignore)
    (provide 'vterm-module))

  :config
  (setq-hook! 'vterm-mode-hook
    confirm-kill-processes nil
    hscroll-margin 0
    )
  )

(use-package! comint
  :config
  )

(use-package! shell-pop)

(use-package! exec-path-from-shell
  :disabled
  )

(use-package! sh-script ; built-in
  :defer t
  :config

  (setq sh-indent-after-continuation 'always)

  ;; [pedantry intensifies]
  (setq-hook! 'sh-mode-hook mode-name "sh")

  ;; recognize function names with dashes in them
  (add-to-list 'sh-imenu-generic-expression
               '(sh (nil "^\\s-*function\\s-+\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*\\(?:()\\)?" 1)
                    (nil "^\\s-*\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*()" 1)))

  ;; `sh-set-shell' is chatty about setting up indentation rules
  (advice-add 'sh-set-shell :around #'doom-shut-up-a)

  ;; 1. Fontifies variables in double quotes
  ;; 2. Fontify command substitution in double quotes
  ;; 3. Fontify built-in/common commands (see `+sh-builtin-keywords')
  (add-hook! 'sh-mode-hook (defun +sh-init-extra-fontification-h ()
                             (font-lock-add-keywords
                              nil `((+sh--match-variables-in-quotes
                                     (1 'font-lock-constant-face prepend)
                                     (2 'font-lock-variable-name-face prepend))
                                    (+sh--match-command-subst-in-quotes
                                     (1 'sh-quoted-exec prepend))
                                    (,(regexp-opt +sh-builtin-keywords 'symbols)
                                     (0 'font-lock-type-face append)))))
             )
  ;; 4. Fontify delimiters by depth
  (add-hook! 'sh-mode-hook
             #'rainbow-delimiters-mode
             #'hs-minor-mode
            )

  ;; autoclose backticks
  (sp-local-pair 'sh-mode "`" "`" :unless '(sp-point-before-word-p sp-point-before-same-p))
  )

(use-package! company-shell
  :when (modulep! :ide company)
  :defer t
  :after sh-script
  :config
  (setq company-shell-delete-duplicates t
        ;; whatis lookups are exceptionally slow on macOS (#5860)
        company-shell-dont-fetch-meta IS-MAC)

  )

;;; config.el ends here
