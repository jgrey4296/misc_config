;;; tools/debugger/config.el -*- lexical-binding: t; -*-

(defer-load! "+vars")

(defer-load! jg-total-bindings "+bindings")
;;; Packages

(use-package! projectile-variable
  :defer t
  :commands (projectile-variable-put
             projectile-variable-get
             projectile-variable-alist
             projectile-variable-plist))

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
  (spec-handling-add! popup
                      '(realgud
                        ("^\\*\\(?:trepanjs:\\(?:g\\|zsh\\|bash\\)db\\|pdb \\)" :size 20 :select nil :quit nil)
                        )
                      )


  (defadvice! +debugger--cleanup-after-realgud-a (&optional buf)
    "Kill command buffer when debugging session ends (which closes its popup)."
    :after #'realgud:terminate
    (when (stringp buf)
      (setq buf (get-buffer buf)))
    (when-let (cmdbuf (realgud-get-cmdbuf buf))
      (let (kill-buffer-hook)
        (kill-buffer buf))))

  ;; Monkey-patch `realgud:run-process' to run in a popup.
  ;; TODO Find a more elegant solution
  ;; FIXME Causes realgud:cmd-* to focus popup on every invocation

  (defadvice! +debugger--realgud-open-in-other-window-a
    (debugger-name script-filename cmd-args minibuffer-history-var &optional no-reset)
    :override #'realgud:run-process
    (let* ((cmd-buf (apply #'realgud-exec-shell debugger-name script-filename
                           (car cmd-args) no-reset (cdr cmd-args)))
           (process (get-buffer-process cmd-buf)))
      (cond ((and process (eq 'run (process-status process)))
             (pop-to-buffer cmd-buf)
             (when (boundp 'evil-emacs-state-local-map)

               (define-key evil-emacs-state-local-map (kbd "ESC ESC") #'+debugger/quit))
             (realgud:track-set-debugger debugger-name)
             (realgud-cmdbuf-info-in-debugger?= 't)
             (realgud-cmdbuf-info-cmd-args= cmd-args)
             (when cmd-buf
               (switch-to-buffer cmd-buf)
               (when realgud-cmdbuf-info
                 (let* ((info realgud-cmdbuf-info)
                        (cmd-args (realgud-cmdbuf-info-cmd-args info))
                        (cmd-str  (mapconcat #'identity cmd-args " ")))
                   (if (boundp 'starting-directory)
                       (realgud-cmdbuf-info-starting-directory= starting-directory))
                   (set minibuffer-history-var
                        (cl-remove-duplicates (cons cmd-str (eval minibuffer-history-var))
                                              :from-end t))))))
            (t
             (if cmd-buf (switch-to-buffer cmd-buf))
             (message "Error running command: %s" (mapconcat #'identity cmd-args " "))))
      cmd-buf)))

(use-package! realgud-trepan-ni
  :defer t
  :init (add-to-list '+debugger--realgud-alist
                     '(realgud:trepan-ni :modes (javascript-mode js2-mode js3-mode)
                       :package realgud-trepan-ni))
  )

(use-package! dap-mode
  :when (and (modulep! +lsp) (not (modulep! :tools lsp +eglot)))
  :hook (dap-mode . dap-tooltip-mode)
  :init
  (setq dap-breakpoints-file (concat doom-data-dir "dap-breakpoints")
        dap-utils-extension-path (concat doom-data-dir "dap-extension/"))
  (after! lsp-mode (require 'dap-mode))
  :config
  (pcase-dolist (`((,category . ,modules) :after ,after :require ,libs)
                 +debugger--dap-alist)
    (when (doom-module-p category (car modules) (cadr modules))
      (dolist (lib (ensure-list after))
        (with-eval-after-load lib
          (mapc #'require (ensure-list libs))))))

  (dap-mode 1)

  ;; Activate this minor mode when dap is initialized
  (add-hook 'dap-session-created-hook #'+dap-running-session-mode)
  ;; Activate this minor mode when hitting a breakpoint in another file
  (add-hook 'dap-stopped-hook #'+dap-running-session-mode)
  ;; Activate this minor mode when stepping into code in another file
  (add-hook 'dap-stack-frame-changed-hook (lambda (session)
                                            (when (dap--session-running session)
                                              (+dap-running-session-mode 1))))

  (map! :localleader
        :map +dap-running-session-mode-map
        "d" #'dap-hydra)
  )

(use-package! dap-ui
  :when (and (modulep! +lsp) (not (modulep! :tools lsp +eglot)))
  :hook (dap-mode . dap-ui-mode)
  :hook (dap-ui-mode . dap-ui-controls-mode)
  )
