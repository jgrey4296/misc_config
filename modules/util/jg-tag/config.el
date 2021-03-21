

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
  :commands (org-ref-bibtex-hydra/body org-ref-bibtex-new-entry/body)
  :init
  (custom-set-variables '(org-ref-insert-cite-key "C-c i"))
  (add-hook 'bibtex-mode-hook #'yas-minor-mode)
  (add-hook 'bibtex-mode-hook #'reftex-mode)
  )


(after! (helm evil)
  (evil-ex-define-cmd "t[ag]"  #'jg-tag-helm-start)
  (evil-ex-define-cmd "to"     #'jg-tag-occurrences)
  (evil-ex-define-cmd "toa"    #'jg-tag-occurrences-in-open-buffers)
  (evil-ex-define-cmd "tv"     #'org-tags-view)
  (evil-ex-define-cmd "ts"     #'org-set-tags)
  (evil-ex-define-cmd "ci[te]" #'jg-tag-insert-simple-bibtex-wrapped)
  )
(after! tag-clean-minor-mode ()
  (push 'tag-clean-minor-mode minor-mode-list)
  )

(after! evil
  (load! "+org-spec-bindings")
  (load! "+bindings")
  )

(after! ivy (load! "+ivy_actions"))

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
