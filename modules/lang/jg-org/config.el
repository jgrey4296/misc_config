;; set pomodoro log variable

(load! "+funcs")
(load! "+pomodoro-funcs")
(load! "+clean-funcs")
(load! "+dired")
(load! "+vars")
(load! "+tags")
(load! "+text-utils")
(after! (evil org)
  (load! "+org-standard-bindings.el")
  (load! "+bindings")
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


(add-hook 'doom-first-input-hook #'+jg-org-setup-tags-hook 100)

(add-hook 'doom-first-input-hook
          #'(lambda () (remove-hook 'org-tab-first-hook
                               #'+org-cycle-only-current-subtree-h))
          )
