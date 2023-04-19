;;; util/text/config.el -*- lexical-binding: t; -*-
(load! "+vars")
(load! "misc/+funcs")
(load! "misc/+pandoc")
(load! "misc/+jq")
(after! jg-bindings-total
  (load! "evil/+operators")
  (load! "evil/+motions")
  (load! "evil/+state")
  (load! "evil/+text-obj")
  (load! "+bindings")
  (load! "+advice")
)
(load! "modes/+derived-modes")

(use-package! academic-phrases :defer t)

(use-package! highlight-parentheses :defer t)

(use-package! helm-wordnet :defer t)

(use-package! evil-string-inflection
  :defer t
  :commands evil-operator-string-inflection
  )

(use-package! lorem-ipsum
  :commands (lorem-ipsum-insert-sentences lorem-ipsum-insert-paragraphs lorem-ipsum-insert-list)
)

(use-package! rainbow-mode
  :defer t
  :init
  (add-hook! 'prog-mode-hook 'rainbow-mode)
)

(use-package! evil-string-inflection
  :defer t
  :commands evil-operator-string-inflection
  )

(use-package! license-templates :defer)

(use-package! lint-result-mode)

(use-package! vundo
  :commands vundo
  )

(use-package! undo-fu
  :defer t
  :config
  ;; Increase undo history limits to reduce likelihood of data loss
  (setq-default
              ;; Increase undo-limits by a factor of ten to avoid emacs prematurely
              ;; truncating the undo history and corrupting the tree. See
              ;; https://github.com/syl20bnr/spacemacs/issues/12110
              undo-limit 800000
              undo-strong-limit 12000000
              undo-outer-limit 120000000
              )
  )

(spec-handling-new-hooks! rotate-text
                          (setq-local rotate-text-local-symbols    (plist-get val :symbols)
                                      rotate-text-local-words      (plist-get val :words)
                                      rotate-text-local-patterns   (plist-get val :patterns)
                                      )
                          )

(spec-handling-new-hooks! whitespace-cleanup
                          (setq-local jg-text-whitespace-clean-hook (ensure-list val))
                          )
