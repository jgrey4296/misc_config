;;; lang/markdown/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(defer-load! jg-bindings-total "+bindings")

(use-package! markdown-mode
  :commands markdown-ode
  :init

  ;; A shorter alias for org src blocks than "markdown"
  (after! org-src
    (add-to-list 'org-src-lang-modes '("md" . markdown)))

  :config
  (add-hook! 'gfm-mode  :depth 100
             #'outline-minor-mode
             #'general-insert-minor-mode
             )

  (sp-local-pair '(markdown-mode gfm-mode) "`" "`"
                 :unless '(:add sp-point-before-word-p sp-point-before-same-p))

  ;; Highly rust blocks correctly
  (when (modulep! :lang rust)
    (add-to-list 'markdown-code-lang-modes '("rust" . rustic-mode)))

  ;; Don't trigger autofill in code blocks (see `auto-fill-mode')
  (setq-hook! 'markdown-mode-hook
    fill-nobreak-predicate (cons #'markdown-code-block-at-point-p
                                 fill-nobreak-predicate))

  ;; HACK Prevent mis-fontification of YAML metadata blocks in `markdown-mode'
  ;;      which occurs when the first line contains a colon in it. See
  ;;      jrblevin/markdown-mode#328.
  (defadvice! +markdown-disable-front-matter-fontification-a (&rest _)
    :override #'markdown-match-generic-metadata
    (ignore (goto-char (point-max))))
)

(use-package! evil-markdown
  :after markdown-mode
  :hook (markdown-mode . evil-markdown-mode)
  :config
  (add-hook 'evil-markdown-mode-hook #'evil-normalize-keymaps)
  )
