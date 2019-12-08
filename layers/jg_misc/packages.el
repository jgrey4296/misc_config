(defconst jg_misc-packages
  '(
    erlang
    fci
    rainbow-mode
    nlinum
    shell

    buffer-utils
    ;; buffer-manage
    ;; (buffer-sets :location (recipe :fetcher git :url "https://git.flintfam.org/swf-projects/buffer-sets.git"))
    ;; (filesets+ :location (recipe :fetcher github :repo "emacsmirror/filesets-plus"))
    ;; helm-filesets

    evil-string-inflection
    free-keys
    fsm
    dired-quick-sort
    highlight-parentheses
    plantuml-mode
    flycheck-plantuml
    undo-tree
    dired
    ;; ggtags
    ;; (helm-gtags :toggle (configuration-layer/package-usedp 'helm))
    ;; xcscope
    ;; (helm-cscope :toggle (configuration-layer/package-usedp 'helm))

    )
  )


;; (defun jg_layer/init-filesets+ ()
;;   (use-package filesets+
;;     :init (filesets-init))
;;   )

;; (use-package highlight-parentheses
;;   :init
;;   (progn
;;     (when (member dotspacemacs-highlight-delimiters '(all current))
;;       (add-hook 'prog-mode-hook #'highlight-parentheses-mode))
;;     (setq hl-paren-delay 0.2)
;;     (spacemacs/set-leader-keys "tCp" 'highlight-parentheses-mode)
;;     (setq hl-paren-colors '("color-16" "color-16" "color-16" "color-16")
;;           hl-paren-background-colors '("Springgreen3" "color-26" "color-91" "IndianRed3")))
;;   :config
;;   (spacemacs|hide-lighter highlight-parentheses-mode)
;;   (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)))

;; (defun jg_layer/init-origami ()
;;   (use-package origami))

(defun jg_misc/post-init-erlang ()
  ;; (also has a load path set in root el file)
  (setq erlang-root-dir "/usr/local/opt/erlang"
        exec-path (cons "/usr/local/opt/erlang/bin" exec-path)
        )
  )

(defun jg_misc/post-init-fci ()
  (add-hook 'change-major-mode-after-body-hook 'fci-mode)
  )

(defun jg_misc/init-rainbow-mode ()
  (use-package rainbow-mode
    :commands (rainbow-mode)
    :config (progn
              ;; (spacemacs/set-leader-keys "t C r" 'rainbow-mode)
              (add-hook 'prog-mode-hook 'rainbow-mode))
    )
  )

(defun jg_misc/init-nlinum ()
  (use-package nlium
    :commands (nlinum-mode)
    )
  )

(defun jg_misc/post-init-shell ()
  ;; shell default can be: shell, eshell, term, ansi-term
  (remove-hook 'term-mode-hook 'spacemacs/disable-hl-line-mode)
  (remove-hook 'comint-mode-hook 'spacemacs/disable-hl-line-mode)
  )

(defun jg_misc/init-buffer-utils ()
  (use-package buffer-utils
    :defer t)
  )

(defun jg_misc/init-evil-string-inflection ()
  (use-package evil-string-inflection
    :config
    (define-key evil-normal-state-map
      "g '" 'evil-operator-string-inflection
      )
    )
  )

(defun jg_misc/init-free-keys ()
  (use-package free-keys
    :config
    (spacemacs/declare-prefix "a U" "Utilities")
    (spacemacs/set-leader-keys
      "a U f k" 'free-keys
      "a U f p" 'free-keys-set-prefix
      )
    )
  )

(defun jg_misc/init-fsm ()
  (use-package fsm
    :defer t)
  )

(defun jg_misc/init-dired-quick-sort ()
  (use-package dired-quick-sort
    :init (dired-quick-sort-setup)
    )
  )

(defun jg_misc/post-init-highlight-parentheses ()
  (setq hl-paren-colors '("color-16" "color-16" "color-16" "color-16")
        hl-paren-background-colors '("Springgreen3" "color-26" "color-91" "IndianRed3"))
  )

(defun jg_misc/init-plantuml-mode ()
  (use-package plantuml-mode
    :defer t
    )
  )

(defun jg_misc/init-flycheck-plantuml ()
  (use-package flycheck-plantuml
    :defer t
    :config
    (flycheck-plantuml-setup)
    )
  )

(defun jg_misc/post-init-undo-tree ()
  (spacemacs/set-leader-keys
    "a U u" 'undo-tree-visualize
    )

  )

(defun jg_misc/post-init-dired ()
  (spacemacs/set-leader-keys
    "a d" nil)
  )
