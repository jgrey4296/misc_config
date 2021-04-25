;; set pomodoro log variable

(load! "+funcs")
(load! "+pomodoro-funcs")
(load! "+clean-funcs")
(load! "+dired")
(load! "+vars")
(load! "+tags")
(load! "+text-utils")
(load! "+org-standard-bindings.el")
(load! "+bindings")

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

(add-hook! org-load
           #'+jg-org-setup-tags-hook
           #'+jg-org-main-bindings-hook
           #'+jg-org-personal-binding-hook
           )

(add-hook! doom-first-input
           #'+jg-org-general-binding-hook

           )

(add-hook! dired-load
           #'+jg-org-dired-binding-hook
           )
