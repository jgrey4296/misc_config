;;; completion/company/config.el -*- lexical-binding: t; -*-

(local-load! "+vars")
(local-load! "+spec-defs")

(defer-load! jg-bindings-total "+bindings")

(use-package! company
  :hook (doom-first-input . global-company-mode)
  :init
  (add-hook 'global-company-mode-hook #'company-tng-mode)

  :config
  (advice-add 'company-begin-backend :before #'+company--abort-previous-a)
  (after! evil
    (add-hook 'company-mode-hook                #'evil-normalize-keymaps)
    (add-hook 'evil-normal-state-entry-hook     #'+company-abort-h)
    (add-hook 'evil-escape-inhibitor-hook       #'company--active-p)
    )

  ;; NOTE Fix #1335: ensure `company-emulation-alist' is the first item of
  ;;      `emulation-mode-map-alists', thus higher priority than keymaps of
  ;;      evil-mode. We raise the priority of company-mode keymaps
  ;;      unconditionally even when completion is not activated. This should not
  ;;      cause problems, because when completion is activated, the value of
  ;;      `company-emulation-alist' is ((t . company-my-keymap)), when
  ;;      completion is not activated, the value is ((t . nil)).
  ;; (add-hook! 'evil-local-mode-hook
  ;;   (when (memq 'company-emulation-alist emulation-mode-map-alists)
  ;;     (company-ensure-emulation-alist)))

  (add-hook! 'minibuffer-inactive-mode-hook :append #'company-mode)
  (add-hook! 'minibuffer-setup-hook         :append #'company-mode)

  ;; Fix #4355: allow eldoc to trigger after completions.
  (after! eldoc
    (eldoc-add-command 'company-complete-selection
                       'company-complete-common
                       'company-capf
                       'company-abort))
)

(use-package! company-abbrev
  :after company
  )

(use-package! company-dabbrev
  :after comapny)

(use-package! company-dict
  :after company
  :config
  (add-hook! 'doom-project-hook
             #'+company-enable-project-dicts-h
             )
  )

(use-package! jg-company
  :commands jg-company/backend
  )
