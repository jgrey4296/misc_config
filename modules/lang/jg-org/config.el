;; set pomodoro log variable

(load! "+funcs")
(load! "+pomodoro-funcs")
(load! "+clean-funcs")
(load! "+dired")
(load! "+vars")
(load! "+tags")
(load! "+text-utils")
(after! evil
  (load! "+org-standard-bindings.el")
  (load! "+org-standard-bindings-2.el")
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

(after! org
  (add-hook! 'org-mode-hook #'+jg-org-setup-tags-hook)
  )
