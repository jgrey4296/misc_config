;;; util/text/config.el -*- lexical-binding: t; -*-
(load! "+vars")
(load! "misc/+funcs")
(load! "misc/+barchart")
(after! jg-bindings-total
  (load! "evil/+operators")
  (load! "evil/+motions")
  (load! "evil/+state")
  ;; (load! "+undo-state")
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
(use-package! wordnut
  :defer t
  :init
  (add-hook 'wordnut-mode-hook 'outline-minor-mode)

  )
(use-package! license-templates)

(use-package! lint-result-mode)
(use-package! vundo
  :commands vundo
  )

(use-package! undo-fu
  :config
  ;; Increase undo history limits to reduce likelihood of data loss
  (setq undo-limit 400000           ; 400kb (default is 160kb)
        undo-strong-limit 3000000   ; 3mb   (default is 240kb)
        undo-outer-limit 48000000)  ; 48mb  (default is 24mb)
  )
