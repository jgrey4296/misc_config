;;; lang/sh/config.el -*- lexical-binding: t; -*-

(defer-load! "+vars")

(use-package! sh-script ; built-in
  :defer t
  :config
  (add-hook 'sh-mode-local-vars-hook #'tree-sitter!)

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
  (add-hook 'sh-mode-hook #'rainbow-delimiters-mode)

  ;; autoclose backticks
  (sp-local-pair 'sh-mode "`" "`" :unless '(sp-point-before-word-p sp-point-before-same-p))
  )

(use-package! company-shell
  :defer t
  :after sh-script
  :config
  (setq company-shell-delete-duplicates t
        ;; whatis lookups are exceptionally slow on macOS (#5860)
        company-shell-dont-fetch-meta IS-MAC)

  )
