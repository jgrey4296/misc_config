;;; transient-toggles.el -*- lexical-binding: t; -*-
(require 'transient)

;; :desc "Input Language" "i" #'toggle-input-method
;; :desc "indent style"   "i" #'doom/toggle-indent-style

(transient-make-toggle! global-prettify-symbols-mode "p" "Pretty Symbols")
(transient-make-toggle! global-company-mode "C" "AutoComplete")
(transient-make-call!   read-only
                        (format "%s  : Read-only" (fmt-as-bool! buffer-read-only))
                        (read-only-mode 'toggle)
                        )
(transient-make-call!   evil-embrace
                        (format "%s  : Evil-Embrace" (fmt-as-bool! (advice-member-p #'evil-embrace-evil-surround-region 'evil-surround-region)))
                        (if (advice-member-p #'evil-embrace-evil-surround-region 'evil-surround-region)
                            (evil-embrace-disable-evil-surround-integration)
                          (evil-embrace-enable-evil-surround-integration))
                        )
(transient-make-toggle! global-flycheck-mode "F" "Flycheck")
(transient-make-call!   run-spec-handlers
                      (let ((str  "Run Spec Handlers"))
                        (put-text-property 0 (length str) 'face 'transient-heading str)
                        str)
                      (run-spec-handlers)
                      )
(transient-make-call!   general-insert-rebuild-cache
                        (let ((str  "Clear General-Insert Cache"))
                          (put-text-property 0 (length str) 'face 'transient-heading str)
                          str)
                        (general-insert-clear-caches)
                        (message "Cache Rebuilt")
                        )
;; Visual
(progn
  (transient-make-toggle! hl-line-mode "h" "Highlight-line")
  (transient-make-toggle! evil-goggles-mode "g" "Evil-goggles")
  (transient-make-toggle! highlight-parentheses-mode "w" "Higlight-wrappers")
  (transient-make-toggle! rainbow-mode "r" "Rainbow Colours")
  (transient-make-toggle! highlight-changes-visible-mode "x" "Highlight-changes")
  (transient-make-call! link-display
                        (format "%s  : Link Display" (fmt-as-bool! org-link-descriptive))
                        (org-toggle-link-display))
  (transient-make-call!   quickscope
                          (format "%s  : Quickscope"
                                  (fmt-as-bool! evil-quickscope-always-mode))
                          (evil-quickscope-always-mode 'toggle)
                          (evil-quickscope-mode (if evil-quickscope-always-mode -1 1))
                          )
  (transient-make-call!   invisible
                          (format "%s  : Invisible Spec" (fmt-as-bool! line-move-ignore-invisible))
                          (setq line-move-ignore-invisible (not line-move-ignore-invisible))
                          )

  )
(transient-make-subgroup! jg-toggle-visuals "v"
  "For controlling ui visual settings"
  :desc "|| Visuals    ||"
  [[
   (jg-transient-toggle-evil-goggles-mode)
   (jg-transient-toggle-hl-line-mode)
   ("i" jg-transient-call-invisible)
   (jg-transient-toggle-highlight-changes-visible-mode)
   ("l" jg-transient-call-link-display)
   ]
  [
   (jg-transient-toggle-rainbow-mode)
   (jg-transient-toggle-global-prettify-symbols-mode)
   ("s" jg-transient-call-quickscope)
   (jg-transient-toggle-highlight-parentheses-mode)
   ]
  ]
  )

;; Guides
(progn
  (transient-make-toggle! display-fill-column-indicator-mode "c" "Columns")
  (transient-make-toggle! highlight-indent-guides-mode "i" "Indents")
  (transient-make-toggle! ruler-mode "r" "Ruler")
  (transient-make-toggle! whitespace-mode "w" "Whitespace")
  (transient-make-call!   spelling
                          (format "%s  : Spelling" (fmt-as-bool! flyspell-mode))
                          (flyspell-mode 'toggle)
                          (writegood-mode (if flypsell-mode 1 -1))
                          )
  (transient-make-toggle! display-line-numbers-mode "n" "Line Numbers")

  )
(transient-make-subgroup! jg-toggle-guides "g"
  "For controlling ui guide settings"
  :desc "|| Guides     ||"
  [[
    (jg-transient-toggle-display-fill-column-indicator-mode)
    (jg-transient-toggle-highlight-indent-guides-mode)
    (jg-transient-toggle-display-line-numbers-mode)
    ] [
    (jg-transient-toggle-ruler-mode)
    (jg-transient-toggle-whitespace-mode)
    ("s" jg-transient-call-spelling)
    ] ]
  )

;; Nav
(progn
  (transient-make-toggle! centered-cursor-mode "c" "Center Cursor")
  (transient-make-toggle! minimap-mode "m" "Minimap")
  (transient-make-toggle! evil-visual-mark-mode "v" "visual mark")
  (transient-make-call! neotree
                        "neotree"
                        (neotree-toggle)
                        )
  (transient-make-call! frame-fullscreen
                        (format "Fullscreen")
                        (toggle-frame-fullscreen))
  (transient-make-call! auto-balance
                        (format "%s  : Auto-Balance Windows" (fmt-as-bool! evil-auto-balance-windows))
                        (setq evil-auto-balance-windows
                              (not evil-auto-balance-windows))
                        )

  )
(transient-make-subgroup! jg-toggle-nav "n"
  "For controlling ui nav settings"
  :desc "|| Navigation ||"
  [ [
    (jg-transient-toggle-centered-cursor-mode)
    ("b" jg-transient-call-auto-balance)
    (jg-transient-toggle-minimap-mode)
    (jg-transient-toggle-evil-visual-mark-mode)
   ] [
    ("n" jg-transient-call-neotree)
    ("f" jg-transient-call-frame-fullscreen)
    ] [
    ] ]
  )

;; Wrap
(progn
  (transient-make-toggle! visual-line-mode "l" "Visual line")
  (transient-make-toggle! +word-wrap-mode "w" "Word-wrap")
  (transient-make-call! truncate-lines
                        (format "%s  : Truncate lines" (fmt-as-bool! truncate-lines))
                        (toggle-truncate-lines)
                        )
  (transient-make-call! auto-fill-mode
                        (format "%s  : Auto-fill" (fmt-as-bool!
                                                  auto-fill-function))
                        (auto-fill-mode 'toggle)
                        )
  )
(transient-make-subgroup! jg-toggle-wrap "w"
  "For controlling ui wrap settings"
  :desc "|| Wrapping   ||"
  [[
    ("f" jg-transient-call-auto-fill-mode)
    (jg-transient-toggle-visual-line-mode)
    (jg-transient-toggle-+word-wrap-mode)
    ("t" jg-transient-call-truncate-lines)
   ]
  ]
  )

;; Debug
(progn
  (transient-make-call! debug-on-error
                        (format "%s  : Debug on Error" (fmt-as-bool! debug-on-error))
                        (toggle-debug-on-error))
  (transient-make-call! debug-on-var
                        (format "%s  : Debug on Variable" (fmt-as-bool! (debug--variable-list)))
                        (call-interactively #'debug-on-variable-change))
  (transient-make-call! cancel-debug-on-var
                        "Cancel Debug on Var"
                        (cancel-debug-on-variable-change))
  (transient-make-call! debug-func
                        (format "%s  : Debug on Function" (fmt-as-bool! (debug--function-list)))
                        (call-interactively #'debug-on-entry))
  (transient-make-call! cancel-debug-func
                        "Cancel Debug on Function"
                        (cancel-debug-on-entry))
  )
(transient-make-subgroup! jg-toggle-debugs "d"
                          " debug toggles "
                          :desc "|| Debug      ||"
                          [ [
                           ("e" jg-transient-call-debug-on-error)
                           ("v" jg-transient-call-debug-on-var)
                           ("f" jg-transient-call-debug-func)
                           ]
                          [
                           " "
                           ("V" jg-transient-call-cancel-debug-on-var)
                           ("F" jg-transient-call-cancel-debug-func)

                           ] ]
                          )

;; Top Level Toggle
(progn
  (transient-make-toggle! hide-mode-line-mode "m" "Modeline")
  (transient-make-toggle! global-hl-line-mode "h" "Hi-line")
  (transient-make-toggle! global-code-shy-minor-mode "H" "Hide Blocks")
  (transient-make-toggle! global-centered-cursor-mode "c" "Center Cursor")
  (transient-make-toggle! global-highlight-changes-mode "x" "Changes")
  (transient-make-toggle! smartparens-global-mode "s" "SmartParens")
  )
;;TODO apply choice
(transient-define-argument browse-selector ()
  :class 'transient-switches
  :argument-format "-browser=%s"
  :argument-regexp "-browser=%s"
  :choices browse-select-variants
  )

;;;###autoload (autoload #'jg-toggle-main "config/ui/autoload/transient-toggles" nil t)
(transient-define-prefix jg-toggle-main ()
  "Main controller for ui settings"
  [ [
     jg-toggle-debugs
     jg-toggle-guides
     jg-toggle-nav
     jg-toggle-visuals
     jg-toggle-wrap
     ]
      [
     (jg-transient-toggle-global-hl-line-mode)
     (jg-transient-toggle-hide-mode-line-mode)
     (jg-transient-toggle-global-prettify-symbols-mode)
     (jg-transient-toggle-global-highlight-changes-mode)
     ] [
     (jg-transient-toggle-global-code-shy-minor-mode)
     (jg-transient-toggle-smartparens-global-mode)
     (jg-transient-toggle-global-centered-cursor-mode)
     (jg-transient-toggle-global-company-mode)
     ] [
     ("r" jg-transient-call-read-only)
     ("E" jg-transient-call-evil-embrace)
     ("e" jg-transient-call-debug-on-error)
     ] ]
  [
   ("!" jg-transient-call-run-spec-handlers)
   ("@" jg-transient-call-general-insert-rebuild-cache)
   ]
  transient-quit!
  )

;;;###autoload
(define-minor-mode transient-toggles-minor-mode
    "  "
    :init-value nil
    :lighter ""
    :global t
    :keymap (make-sparse-keymap)
)
