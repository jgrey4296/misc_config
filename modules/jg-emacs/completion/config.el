(load! "+vars")
(load! "+funcs")
(load! "+snippet-fix")
(load! "+advice")
(load! "+file-templates")

(after! jg-bindings-total
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

(defun +jg-completion-on-load-hook ()
  (require 'yasnippet)
  (advice-add '+snippet--completing-read-uuid :override #'+jg-completion-snippet--completing-read-uuid)
  (add-hook 'yas-prompt-functions #'+jg-yas-prompt-fn -90)

  (after! doom-snippets
    (+jg-completion-activate-file-templates t)
    (yas-reload-all)
    )
  )
(add-hook 'doom-first-file-hook #'+jg-completion-on-load-hook)

(after! 'helm
  (load! "+helms")
  )
