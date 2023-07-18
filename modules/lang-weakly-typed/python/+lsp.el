;;; +lsp.el -*- lexical-binding: t; -*-

(doom-log "Loading Python LSP")

(use-package! lsp-pyright
  :after (python-mode lsp-mode)
  :preface
  (defun +jg-python-pyright-activate (state)
    (when (plist-get state :name)
      (setq lsp-pyright-extra-paths (vector python-shell-extra-pythonpaths
                                            (f-join (plist-get state :path)
                                                    (plist-get state :name))))
      (add-hook 'python-mode-hook #'lsp-deferred)
      )
    )

  (defun +jg-python-pyright-deactivate (state)
    (when (and (boundp 'lsp-mode) lsp-mode)
      (lsp-mode -1))
    (when (fboundp 'lsp--last-active-workspaces)
      (lsp-workspace-shutdown (car lsp--last-active-workspaces)))
    (remove-hook 'python-mode-hook #'lsp-deferred)
    (setq lsp-pyright-extra-paths #'[])
    )

  (defun +jg-python-pyright-teardown (state)
    (lsp-disconnect)
    )

  (spec-handling-add! python-env
                      `(pyright
                        (:support pyright
                                  ,#'+jg-python-pyright-activate
                                  ,#'+jg-python-pyright-deactivate
                                  )
                        (:teardown pyright ,#'+jg-python-pyright-teardown)
                      )
  )
  )

(use-package! lsp-python-ms
  :disabled
  :after (python-mode lsp-mode)
  :init
  (add-to-list 'lsp-disabled-clients 'python-ms)
  :config
  (setq lsp-python-ms-python-executable-cmd python-shell-interpreter)
  )

(use-package! lsp-jedi
  :disabled
  :after (python-mode lsp-mode)
  :init
  (add-to-list 'lsp-disabled-clients 'jedi)
  )
