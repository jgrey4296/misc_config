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
(after! helm-mode
  (map! :map helm-map
        "<tab>" nil
        "TAB" #'helm-select-action
        )
  )
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

(defun jg-load-templates-hook ()
  (load! "+file-templates")
  (add-function :override (symbol-function '+snippet--completing-read-uuid) #'+jg-snippet--completing-read-uuid)
  )
(add-hook 'doom-first-input-hook #'jg-load-templates-hook)
