;;; transient-toggles.el -*- lexical-binding: t; -*-
(require 'macro-tools--transient)

;; :desc "Input Language" "i" #'toggle-input-method
;; :desc "indent style"   "i" #'doom/toggle-indent-style

;; Top Level Toggle

(transient-toggle-hook! company ()
  "AutoComplete"
  :key "C"
  :global t
  :hook '(prog-mode text-mode)
  :fn #'company-mode
  )
(transient-toggle-mode! read-only-mode ()
  "Read Only"
  :key "r"
  :mode-var buffer-read-only
  )
;; TODO remove this, use envs hooks
(transient-toggle-hook! flycheck  ()
  "Flycheck"
  :key "f"
  :global t
  :hook prog-mode-hook
  :fn #'flycheck-mode
  )
(transient-toggle-hook! smartparens-mode ()
  "SmartParens"
  :key "s"
  :global t
  :hook prog-mode-hook
  :fn #'smartparens-mode
  )

(transient-call! uptime ()
  "Emacs Uptime"
  :key "u"
  (message "Uptime: %s" (emacs-uptime))
  )
(transient-call! evil-embrace ()
  "Evil-Embrace"
  :key "E"
  :desc (transient-mode-fmt "Evil-Embrace"
                            (not (null (advice-member-p #'evil-embrace-evil-surround-region 'evil-surround-region)))
                            "e")
  (if (advice-member-p #'evil-embrace-evil-surround-region 'evil-surround-region)
      (evil-embrace-disable-evil-surround-integration)
    (evil-embrace-enable-evil-surround-integration))
  )
(transient-call! run-spec-handlers ()
  "Run Handlers"
  :key "!"
  :desc (propertize "Run Spec Handlers" 'face 'transient-heading)
  (speckler-go!)
  )
(transient-toggle-hook! code-shy ()
  "Shy Code"
  :global t
  :key "H"
  :hook '(prog-mode toml-mode)
  :fn #'code-shy-minor-mode
  )
(transient-toggle-mode! hide-mode-line-mode ()
  "Hide Modeline"
  :key "m"
  )
(transient-toggle-mode! highlight-changes-mode ()
  "Show Changes"
  :key "x"
  )
(transient-toggle-mode! abbrev-mode ()
  "Abbrev"
  :key "a"
  )

;;;###autoload
(defun +jg-ui-build-main-toggle-transient ()
  (transient-define-prefix jg-toggle-main ()
    "Main controller for ui settings"
    [["Global Triggers"
      (transient-macro-call-run-spec-handlers)
      (transient-macro-call-uptime)
      ]
     [""
      ]
     ]
    ["Subsections" []]
    [[""
      (transient-macro-toggle-hook-smartparens-mode)
      (transient-macro-toggle-hook-flycheck)
      (transient-macro-toggle-hook-company)
      ]
     [""
      (transient-macro-toggle-hook-hl-line)
      (transient-macro-toggle-hook-code-shy)
      (transient-macro-toggle-hook-centered-cursor)
      ]
     ["Local Toggles"
      (transient-macro-toggle-highlight-changes-mode)
      (transient-macro-toggle-abbrev-mode)
      (transient-macro-toggle-hide-mode-line-mode)
      (transient-macro-toggle-read-only-mode)
      (transient-macro-call-evil-embrace)
      ]
     ]
    macro-tools--transient-quit!
    )
  )

;;;###autoload
(define-minor-mode transient-toggles-minor-mode
  "  "
  :init-value nil
  :lighter ""
  :global t
  :keymap (make-sparse-keymap)
  (evil-define-key 'normal transient-toggles-minor-mode-map
    "T" #'jg-toggle-main
    )
  (evil-make-overriding-map transient-toggles-minor-mode-map)
  )

;;;###autoload
(defun +jg-ui-rebuild-transient-toggles ()
  (interactive)
  ;; Build the main
  (+jg-ui-build-main-toggle-transient)
  ;; Each hook builds a suffix and appends it
  )

;;;###autoload (autoload 'jg-ui-transient-toggles-builder "config/ui/autoload/transient-toggle-main" nil t)
(transient-setup-hook! jg-ui-transient-toggles ()
  (+jg-ui-build-main-toggle-transient)
  )
