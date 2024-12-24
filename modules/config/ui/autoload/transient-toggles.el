;;; transient-toggles.el -*- lexical-binding: t; -*-
(require 'transient-macros)

;; :desc "Input Language" "i" #'toggle-input-method
;; :desc "indent style"   "i" #'doom/toggle-indent-style

(defvar jg-transient-toggles-hook nil)

;; Top Level Toggle

(transient-toggle-mode! global-company-mode ()
  "AutoComplete"
  :key "C"
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
  :hook prog-mode-hook
  :fn #'flycheck-mode
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
(transient-call! general-insert-rebuild-cache ()
  ""
  :key "@"
  :desc (propertize "Clear General-Insert Cache" 'face 'transient-heading)
  (librarian-insert-clear-caches)
  (message "Cache Rebuilt")
  )
(transient-toggle-mode! global-hl-line-mode ()
  "Hl-line"
  :key "h"
  )
(transient-toggle-mode! hide-mode-line-mode ()
  "Hide Modeline"
  :key "m"
  )
(transient-toggle-mode! global-code-shy-minor-mode ()
  "Shy Code"
  :key "H"
  )
(transient-toggle-mode! global-centered-cursor-mode ()
  "Center Cursor"
  :key "c"
  )
(transient-toggle-mode! global-highlight-changes-mode ()
  "Show Changes"
  :key "x"
  )
(transient-toggle-mode! smartparens-global-mode ()
  "SmartParens"
  :key "s"
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
      ]
     [""
      (transient-macro-call-general-insert-rebuild-cache)
      ]
     ]
    ["Subsections" []]
    [["Global Toggles"
      (transient-macro-toggle-hook-flycheck)
      ;; (transient-macro-toggle-global-prettify-symbols-mode)
      (transient-macro-toggle-global-highlight-changes-mode)
      (transient-macro-toggle-smartparens-global-mode)
      ]
     [""
      (transient-macro-toggle-global-hl-line-mode)
      (transient-macro-toggle-global-code-shy-minor-mode)
      (transient-macro-toggle-global-centered-cursor-mode)
      (transient-macro-toggle-global-company-mode)
      ]
     ["Local Toggles"
      (transient-macro-toggle-abbrev-mode)
      (transient-macro-toggle-hide-mode-line-mode)
      (transient-macro-toggle-read-only-mode)
      (transient-macro-call-evil-embrace)
      ]
     ]
    transient-quit!
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
  (run-hooks 'jg-transient-toggles-hook)
  )

;; (transient-setup-hook! jg-ui-transient-toggles ()
;;   (+jg-ui-build-main-toggle--transient)
;;   )
