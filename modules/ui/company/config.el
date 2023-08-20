;;; completion/company/config.el -*- lexical-binding: t; -*-

(load! "+vars")
(load! "+spec-defs")
(defer-load! jg-bindings-total "+bindings")
(add-hook! 'minibuffer-inactive-mode-hook :append #'company-mode)
(add-hook! 'minibuffer-setup-hook :append #'company-mode)

(use-package! company
  :commands (company-complete-common
             company-complete-common-or-cycle
             company-manual-begin
             company-grab-line)
  :hook (doom-first-input . global-company-mode)

  :init
  (when (modulep! +tng)
    (add-hook 'global-company-mode-hook #'company-tng-mode))

  :config
  (after! evil
    (add-hook 'company-mode-hook #'evil-normalize-keymaps)
    )
  (unless (modulep! +childframe)
    ;; Don't persist company popups when switching back to normal mode.
    ;; `company-box' aborts on mode switch so it doesn't need this.
    (add-hook! 'evil-normal-state-entry-hook
      (defun +company-abort-h ()
        ;; HACK `company-abort' doesn't no-op if company isn't active; causing
        ;;      unwanted side-effects, like the suppression of messages in the
        ;;      echo-area.
        ;; REVIEW Revisit this to refactor; shouldn't be necessary!
        (when company-candidates
          (company-abort)))))

  ;; Allow users to switch between backends on the fly. E.g. C-x C-s followed
  ;; by C-x C-n, will switch from `company-yasnippet' to
  ;; `company-dabbrev-code'.
  (defadvice! +company--abort-previous-a (&rest _) :before #'company-begin-backend
    (company-abort))

  ;; (add-hook 'after-change-major-mode-hook #'+company-init-backends-h 'append)

  (add-hook! 'evil-local-mode-hook
             ;; NOTE Fix #1335: ensure `company-emulation-alist' is the first item of
             ;;      `emulation-mode-map-alists', thus higher priority than keymaps of
             ;;      evil-mode. We raise the priority of company-mode keymaps
             ;;      unconditionally even when completion is not activated. This should not
             ;;      cause problems, because when completion is activated, the value of
             ;;      `company-emulation-alist' is ((t . company-my-keymap)), when
             ;;      completion is not activated, the value is ((t . nil)).
    (when (memq 'company-emulation-alist emulation-mode-map-alists)
      (company-ensure-emulation-alist)))

  ;; Fix #4355: allow eldoc to trigger after completions.
  (after! eldoc
    (eldoc-add-command 'company-complete-selection
                       'company-complete-common
                       'company-capf
                       'company-abort))
)

(use-package! company-dict
  :defer t
  :config
  (add-hook! 'doom-project-hook
    (defun +company-enable-project-dicts-h (mode &rest _)
      "Enable per-project dictionaries."
      (if (symbol-value mode)
          (add-to-list 'company-dict-minor-mode-list mode nil #'eq)
        (setq company-dict-minor-mode-list (delq mode company-dict-minor-mode-list)))))

  )

(use-package! jg-company
  :commands jg-company/backend
  )
