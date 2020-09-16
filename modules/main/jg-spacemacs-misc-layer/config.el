(setq-default shell-default-shell 'shell
              shell-protect-eshell-prompt 0
              shell-enable-smart-eshell t
              )

(defun jg-spacemacs-misc-layer/post-init-erlang ()
  ;; (also has a load path set in root el file)
  (setq erlang-root-dir "/usr/local/opt/erlang"
        exec-path (cons "/usr/local/opt/erlang/bin" exec-path)
        )
  )

(defun jg-spacemacs-misc-layer/post-init-fci ()
  (add-hook 'change-major-mode-after-body-hook 'fci-mode)
  )

(defun jg-spacemacs-misc-layer/init-rainbow-mode ()
  (use-package rainbow-mode
    :commands (rainbow-mode)
    :config (progn
              ;; (spacemacs/set-leader-keys "t C r" 'rainbow-mode)
              (add-hook 'prog-mode-hook 'rainbow-mode))
    )
  )

(defun jg-spacemacs-misc-layer/init-nlinum ()
  (use-package nlium
    :commands (nlinum-mode)
    )
  )

(defun jg-spacemacs-misc-layer/post-init-shell ()
  ;; shell default can be: shell, eshell, term, ansi-term
  (remove-hook 'term-mode-hook 'spacemacs/disable-hl-line-mode)
  (remove-hook 'comint-mode-hook 'spacemacs/disable-hl-line-mode)
  )

(defun jg-spacemacs-misc-layer/init-buffer-utils ()
  (use-package buffer-utils
    :defer t)
  )

(defun jg-spacemacs-misc-layer/init-evil-string-inflection ()
  (use-package evil-string-inflection
    :config
    (define-key evil-normal-state-map
      "g '" 'evil-operator-string-inflection
      )
    )
  )

(defun jg-spacemacs-misc-layer/init-free-keys ()
  (use-package free-keys
    :config
    (spacemacs/declare-prefix "a U" "Utilities")
    (spacemacs/set-leader-keys
      "a U f k" 'free-keys
      "a U f p" 'free-keys-set-prefix
      )
    )
  )

(defun jg-spacemacs-misc-layer/init-fsm ()
  (use-package fsm
    :defer t)
  )

(defun jg-spacemacs-misc-layer/init-dired-quick-sort ()
  (use-package dired-quick-sort
    :init (dired-quick-sort-setup)
    )
  )

(defun jg-spacemacs-misc-layer/post-init-highlight-parentheses ()
  (setq hl-paren-colors '("color-16" "color-16" "color-16" "color-16")
        hl-paren-background-colors '("Springgreen3" "color-26" "color-91" "IndianRed3"))
  )

(defun jg-spacemacs-misc-layer/init-plantuml-mode ()
  (use-package plantuml-mode
    :defer t
    )
  )

(defun jg-spacemacs-misc-layer/init-flycheck-plantuml ()
  (use-package flycheck-plantuml
    :defer t
    :config
    (flycheck-plantuml-setup)
    )
  )

(defun jg-spacemacs-misc-layer/post-init-undo-tree ()
  (spacemacs/set-leader-keys
    "a U u" 'undo-tree-visualize
    )

  )

(defun jg-spacemacs-misc-layer/post-init-dired ()
  (spacemacs/set-leader-keys
    "a d" nil)
  )
