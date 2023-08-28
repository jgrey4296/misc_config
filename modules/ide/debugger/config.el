;;; tools/debugger/config.el -*- lexical-binding: t; -*-

(local-load! "+defs")
(local-load! "+vars")

(defer-load! jg-total-bindings "+bindings")

(defer-load! jg-evil-ex-bindings "+evil-ex")

;;-- projectile

(use-package! projectile-variable
  :defer t
  :commands (projectile-variable-put
             projectile-variable-get
             projectile-variable-alist
             projectile-variable-plist))

;;-- end projectile

;;-- realgud

(use-package! realgud
  :defer t
  :init
  ;; Realgud doesn't generate its autoloads properly so we do it ourselves
  (dolist (debugger +debugger--realgud-alist)
    (autoload (car debugger)
      (if-let (sym (plist-get (cdr debugger) :package))
          (symbol-name sym)
        "realgud")
      nil t))

  :config

  )

(use-package! realgud-trepan-ni
  :defer t
  :init
  (add-to-list '+debugger--realgud-alist
               '(realgud:trepan-ni :modes (javascript-mode js2-mode js3-mode)
                 :package realgud-trepan-ni))
  )

(use-package! realgud-lldb
  :after realgud)

(use-package! realgud-ipdb
  :after realgud)

(use-package! realgud-jdb
  :after realgud)

(use-package! realgud-node-inspect
  :after realgud)

(use-package! realgud-node-debug
  :after realgud)

;;-- end realgud

;;-- dap

(use-package! dap-mode
  :when (and (modulep! +lsp) (not (modulep! :tools lsp +eglot)))
  :hook (dap-mode . dap-tooltip-mode)
  :init

  (setq dap-breakpoints-file (concat doom-data-dir "dap-breakpoints")
        dap-utils-extension-path (concat doom-data-dir "dap-extension/"))

  :config
  (pcase-dolist (`((,category . ,modules) :after ,after :require ,libs)
                 +debugger--dap-alist)
    (when (doom-module-p category (car modules) (cadr modules))
      (dolist (lib (ensure-list after))
        (with-eval-after-load lib
          (mapc #'require (ensure-list libs))))))

  ;; Activate this minor mode when dap is initialized
  (add-hook 'dap-session-created-hook #'+dap-running-session-mode)
  ;; Activate this minor mode when hitting a breakpoint in another file
  (add-hook 'dap-stopped-hook #'+dap-running-session-mode)
  ;; Activate this minor mode when stepping into code in another file
  (add-hook 'dap-stack-frame-changed-hook #'+debugger-dap-start-on-stack)

  )

(use-package! dap-ui
  :hook (dap-mode . dap-ui-mode)
  :hook (dap-ui-mode . dap-ui-controls-mode)
  )

;;-- end dap

(use-package! pdb-capf)

(use-package! edebug-x)

(use-package! font-lock-studio)
