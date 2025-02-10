;; +spelling.el -*- lexical-binding: t; -*-

(use-package! flyspell ; built-in
  :defer t
  :preface
  ;; `flyspell' is loaded at startup. In order to lazy load its config we need
  ;; to pretend it isn't loaded.
  (defer-feature! flyspell flyspell-mode flyspell-prog-mode)
  :config
  (provide 'ispell)
  (add-hook! 'flyspell-mode-hook #'+spell-inhibit-duplicate-detection-maybe-h)
  )

(use-package! ispell
  :preface
  (defer-feature! ispell ispell-minor-mode)
  :config
  (add-hook 'text-mode-hook #'+spell-remove-run-together-switch-for-aspell-h)

  (setq ispell-aspell-dict-dir     (ispell-get-aspell-config-value "dict-dir")
        ispell-aspell-data-dir     (ispell-get-aspell-config-value  "data-dir")
        )
  )

(use-package! spell-fu
  :defer t
  :when (executable-find "aspell")
  :init
  :config
  (add-hook 'spell-fu-mode-hook #'+spell-init-excluded-faces-h)
  )

(use-package! flyspell-correct
  :commands flyspell-correct-previous
  :after flyspell
  :config
  (require 'flyspell-correct-ivy nil t)
  )

(use-package! flyspell-lazy
  :after flyspell
  :config
  ;; Fix #3357: flyspell-lazy inhibits flyspell entirely in message-mode
  ;; derivatives (e.g. for notmuch users).
  (setq-hook! 'message-mode-hook flyspell-lazy-disallow-buffers nil)
  )

(use-package! writegood-mode)

(use-package! accent)
