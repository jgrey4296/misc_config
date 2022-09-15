(load! "+vars")
(load! "+funcs")
(load! "+snippet-fix")
(load! "+advice")

(after! evil
  (load! "+bindings")
)
(after! (ivy counsel)
  (load! "+ivys")
  )
(use-package! helm
  :config
  (setq helm-completing-read-handlers-alist nil)
  )
(use-package! helm-files)

(after! (company minibuffer)
  (add-hook! 'minibuffer-inactive-mode-hook :append #'company-mode)
  (add-hook! 'minibuffer-mode-hook :append #'company-mode)
  )
(after! (company gtags helm-gtags python)
  (set-company-backend! 'python-mode 'company-gtags)
  )

(use-package! helm-gtags :defer t)

(after! (yasnippet)
  (load! "+file-templates")
  (defun +jg-completion-on-load-hook ()
    (advice-add '+snippet--completing-read-uuid :override #'+jg-completion-snippet--completing-read-uuid)
    )
  (add-hook 'doom-first-input-hook #'+jg-completion-on-load-hook)
  )


(after! 'helm
  (load! "+helms")
  )
