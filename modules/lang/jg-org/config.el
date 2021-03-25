;; set pomodoro log variable

(load! "+funcs")
(load! "+pomodoro-funcs")
(load! "+clean-funcs")
(load! "+dired")
(load! "+vars")
(after! evil
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
