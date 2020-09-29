

(use-package! tag-clean-minor-mode
  :defer t)
(use-package! tag-mode
  :defer t)
(use-package! helm-bibtex
  :defer t
  :commands (bibtex-completion-init)
)
(use-package! org-ref
  :defer t
  :commands (org-ref-bibtex-hydra/body)
  :init

  )


(after! (helm evil)
  (evil-ex-define-cmd "t[ag]" #'jg-tag-helm-start)
  (evil-ex-define-cmd "to" #'jg-tag-occurrences)
  (evil-ex-define-cmd "toa" #'jg-tag-occurrences-in-open-buffers)
  (evil-ex-define-cmd "tv"  #'org-tags-view)
  (evil-ex-define-cmd "ts"  #'org-set-tags)
  )
(after! tag-clean-minor-mode ()
  (push 'tag-clean-minor-mode minor-mode-list)
  )

(load! "+vars")
(load! "+bibtex")
(load! "+dired")
(load! "+file")
(load! "+helm")
(load! "+index")
(load! "+json")
(load! "+org")
(load! "+tags")
(load! "+util")
(load! "+org-ref-funcs")
(load! "+bindings")
