;;; editor/snippets/config.el -*- lexical-binding: t; -*-

(local-load! "+defs")
(local-load! "+vars")
(local-load! "+spec-defs")
(defer-load! (yasnippet jg-bindings-total) "+bindings")
(after! (doom-snippets yasnippet-snippets)
  (when (functionp 'snippets-spec-set)
    (snippets-spec-set)
    ))

(add-hook 'doom-switch-buffer-hook #'+file-templates-maybe-expand-h)

(use-package! yasnippet
  :defer-incrementally eldoc easymenu help-mode
  :commands (yas-minor-mode yas-minor-mode-on yas-expand yas-expand-snippet yas-lookup-snippet yas-insert-snippet yas-new-snippet yas-visit-snippet-file yas-activate-extra-mode yas-deactivate-extra-mode yas-maybe-expand-abbrev-key-filter)
  :init
  ;; Lazy load yasnippet until it is needed
  (add-transient-hook! #'company-yasnippet (require 'yasnippet))

  :config
  (add-to-list 'doom-debug-variables '(yas-verbosity . 3))

  ;; HACK In case `+snippets-dir' and `doom-snippets-dir' are the same, or duplicates exist in `yas-snippet-dirs'.
  (advice-add 'yas-snippet-dirs :filter-return #'delete-dups)

  ;; Remove GUI dropdown prompt (prefer ivy/helm)
  (delq! 'yas-dropdown-prompt yas-prompt-functions)
  ;; Prioritize private snippets in `+snippets-dir' over built-in ones if there
  ;; are multiple choices.
  (add-to-list 'yas-prompt-functions #'+snippets-prompt-private)

  ;; Register `def-project-mode!' modes with yasnippet. This enables project
  ;; specific snippet libraries (e.g. for Laravel, React or Jekyll projects).
  (add-hook 'doom-project-hook #'+snippets-enable-project-modes-h)

  ;; Exit snippets on ESC from normal mode
  (add-hook 'doom-escape-hook #'yas-abort-snippet)

  (after! smartparens ;; tell smartparens overlays not to interfere with yasnippet keybinds
    (advice-add 'yas-expand :before #'sp-remove-active-pair-overlay))

  ;; (Evil only) fix off-by-one issue with line-wise visual selections in
  ;; `yas-insert-snippet', and switches to insert mode afterwards.
  (advice-add 'yas-insert-snippet :around #'+snippets-expand-on-region-a)

  ;; Show keybind hints in snippet header-line
  (add-hook 'snippet-mode-hook #'+snippets-show-hints-in-header-line-h)
  ;; Enable `read-only-mode' for built-in snippets (in `doom-local-dir')
  (add-hook 'snippet-mode-hook #'+snippets-read-only-maybe-h)

  ;; HACK Smartparens will interfere with snippets expanded by `hippie-expand`, so temporarily disable smartparens during snippet expansion.
  (after! hippie-exp
    ;; Is called for all snippet expansions,
    (add-hook! 'yas-before-expand-snippet-hook #'+snippets--disable-smartparens-before-expand-h)
    )
    (add-hook! 'yas-after-exit-snippet-hook #'+snippets--restore-smartparens-after-expand-h)

  ;; If in a daemon session, front-load this expensive work:
  (yas-global-mode +1)
  )

(use-package! yasnippet-snippets
  :disabled t
  :after yasnippet
  :config
  ;; (push yasnippet-snippets-dir jg-snippet-dirs)
  )

(use-package! doom-snippets
  :disabled t
  :after yasnippet
  :config
  ;; (push doom-snippets-dir jg-snippet-dirs)
  )

(use-package! auto-yasnippet
  :after yasnippet
  )

(use-package! academic-phrases :defer t)

(use-package! license-templates :defer t)

(use-package! lorem-ipsum
  :commands (lorem-ipsum-insert-sentences lorem-ipsum-insert-paragraphs lorem-ipsum-insert-list)
)

(use-package! dabbrev :defer t)
(use-package! abbrev
  :defer t
  )
