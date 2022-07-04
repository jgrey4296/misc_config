(load! "+vars")
(load! "+helm-funcs")
(load! "+funcs")
(load! "+snippet-fix")

(after! evil
  (load! "+bindings")
)
(after! (ivy counsel)
  (load! "+ivy_actions")
  )
(use-package! helm
  :config
  (setq helm-completing-read-handlers-alist nil)
  )
(use-package! helm-files)

(after! company
  (add-hook! 'minibuffer-inactive-mode-hook :append #'company-mode)
  (add-hook! 'minibuffer-mode-hook :append #'company-mode)
  )
(after! (company gtags helm-gtags)
  (set-company-backend! 'python-mode 'company-gtags)
  )

(use-package! helm-gtags :defer t)

(after! (yasnippet)
  (load! "+file-templates")
  (defun jg-completion-on-load-hook ()
    (advice-add '+snippet--completing-read-uuid :override #'+jg-snippet--completing-read-uuid)
    )
  (add-hook 'doom-first-input-hook #'jg-completion-on-load-hook)
  )
