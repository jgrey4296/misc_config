;; set pomodoro log variable

(load! "+funcs")
(load! "+pomodoro-funcs")
(load! "+clean-funcs")
(load! "+dired")
(load! "+vars")
(load! "+tags")
(after! (jg-bindings-total jg-org-stage evil-org)
  (message "Setting up org bindings")
  (load! "+text-utils")
  (load! "+org-standard-bindings.el")
  (load! "+bindings")

  ;; (evil-make-overriding-map org-mode-map)
  (setq minor-mode-map-alist (assq-delete-all 'evil-org-mode minor-mode-map-alist))
  (push `(evil-org-mode . ,evil-org-mode-map) minor-mode-map-alist)
  )

(use-package! link-hint
  :config
  ;; override default org link to open externally sometimes
  (link-hint-define-type 'org-link
    :next #'link-hint--next-org-link
    :at-point-p #'link-hint--org-link-at-point-p
    :vars '(org-mode org-agenda-mode org-link-minor-mode)
    :open #'+jg-org-link-hint-external
    :open-multiple t
    :copy #'kill-new)
  (push 'org-link link-hint-types)
  )

(use-package! graphviz-dot-mode
  :defer t
  :after org
  :init
  (push '("dot" . graphviz-dot) org-src-lang-modes)
  )

(use-package-hook! org :post-config
  (message "post configuring org")
  ;; (setq org-mode-map      (make-sparse-keymap "jg-org")
  ;;       evil-org-mode-map (make-sparse-keymap "jg-evil-org"))
  (provide 'jg-org-stage)
  (add-hook! 'org-mode-hook #'+jg-org-custom-hook)
  )


(add-hook 'doom-first-file-hook #'+jg-org-setup-tags-hook 100)
;; (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h)
