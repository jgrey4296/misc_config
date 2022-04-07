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
  )
(after! (company gtags helm-gtags)
  (set-company-backend! 'python-mode 'company-gtags)
  )
(after! (yasnippet doom-snippets yasnippet-snippets)
  (setq yas-snippet-dirs '(+snippets-dir doom-snippets-dir +file-templates-dir yasnippet-snippets-dir))
  (setq yas--default-user-snippets-dir yas-snippet-dirs)
 )

(use-package! helm-gtags :defer t)

(defun jg-completion-on-load-hook ()
  (load! "+file-templates")
  (advice-add '+snippet--completing-read-uuid :override #'+jg-snippet--completing-read-uuid)
  )
(add-hook 'doom-first-input-hook #'jg-completion-on-load-hook)
