;;; lang/markdown/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(use-package! markdown-mode
  :commands markdown-ode
  :init

  :config
  (add-hook! 'gfm-mode-hook  :depth 100
             #'outline-minor-mode
             #'librarian-insert-minor-mode
             )

  (sp-local-pair '(markdown-mode gfm-mode) "`" "`"
                 :unless '(:add sp-point-before-word-p sp-point-before-same-p))

  (when (modulep! :lang rust)
    (add-to-list 'markdown-code-lang-modes '("rust" . rustic-mode))
    )

  ;; Don't trigger autofill in code blocks (see `auto-fill-mode')
  (setq-hook! 'markdown-mode-hook
    fill-nobreak-predicate (cons #'markdown-code-block-at-point-p
                                 fill-nobreak-predicate))

 (advice-add 'markdown-match-generic-metadata :override #'+markdown-disable-front-matter-fontification-a)
)

(use-package! markdown-ts-mode
  :defer t
  )

(use-package! evil-markdown
  :when (modulep! :editor evil)
  :disabled t
  :after markdown-mode
  ;; :hook (markdown-mode . evil-markdown-mode)
  :config
  (add-hook 'evil-markdown-mode-hook #'evil-normalize-keymaps)
  )

(use-package! grip-mode
  :defer t
  )

(use-package! ox-gfm
  :when (modulep! :lang-tex org)
  )

(use-package! auto-org-md
  :defer t
  )
