;;; transient-toggles.el -*- lexical-binding: t; -*-
(require 'transient)

;; :desc "Input Language" "i" #'toggle-input-method
;; :desc "indent style"   "i" #'doom/toggle-indent-style

(defvar jg-transient-toggles-hook nil)

;; Top Level Toggle
(progn

  (transient-make-mode-toggle! global-company-mode           "AutoComplete"   "C")
  (transient-make-mode-toggle! read-only-mode                "Read Only"      "r" nil buffer-read-only)
  (transient-make-mode-toggle! global-flycheck-mode          "Flycheck"       "f")

  (transient-make-call!   evil-embrace "E"
                          (transient-title-mode-formatter "Evil-Embrace" (advice-member-p #'evil-embrace-evil-surround-region 'evil-surround-region) "e")
                          (if (advice-member-p #'evil-embrace-evil-surround-region 'evil-surround-region)
                              (evil-embrace-disable-evil-surround-integration)
                            (evil-embrace-enable-evil-surround-integration))
                          )
  (transient-make-call!   run-spec-handlers "!"
                          (propertize "Run Spec Handlers" 'face 'transient-heading)
                          (speckler-go!)
                          )
  (transient-make-call!   general-insert-rebuild-cache "@"
                          (propertize    "Clear General-Insert Cache" 'face 'transient-heading)
                          (general-insert-clear-caches)
                          (message "Cache Rebuilt")
                          )
  (transient-make-mode-toggle! global-hl-line-mode            "Hl-line"       "h")
  (transient-make-mode-toggle! hide-mode-line-mode            "Hide Modeline" "m")
  (transient-make-mode-toggle! global-code-shy-minor-mode     "Shy Code"      "H")
  (transient-make-mode-toggle! global-centered-cursor-mode    "Center Cursor" "c")
  (transient-make-mode-toggle! global-highlight-changes-mode  "Show Changes"  "x")
  (transient-make-mode-toggle! smartparens-global-mode        "SmartParens"   "s")
  (transient-make-mode-toggle! abbrev-mode                    "Abbrev"        "a")
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
      (transient-macro-toggle-global-flycheck-mode)
      (transient-macro-toggle-global-prettify-symbols-mode)
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
