;;; transient-toggles.el -*- lexical-binding: t; -*-
(require 'transient)

;; :desc "Input Language" "i" #'toggle-input-method
;; :desc "indent style"   "i" #'doom/toggle-indent-style

(transient-make-mode-toggle! global-prettify-symbols-mode  "Pretty Symbols" "p")
(transient-make-mode-toggle! global-company-mode           "AutoComplete"   "C")
(transient-make-mode-toggle! read-only-mode                "Read Only"      "r" nil buffer-read-only)
(transient-make-mode-toggle! global-flycheck-mode          "Flycheck"       "F")

(transient-make-call!   evil-embrace "E"
                        (format "%-2s : Evil-Embrace" (fmt-as-bool! (advice-member-p #'evil-embrace-evil-surround-region 'evil-surround-region)))
                        (if (advice-member-p #'evil-embrace-evil-surround-region 'evil-surround-region)
                            (evil-embrace-disable-evil-surround-integration)
                          (evil-embrace-enable-evil-surround-integration))
                        )
(transient-make-call!   run-spec-handlers "!"
                        (let ((str  "Run Spec Handlers"))
                          (put-text-property 0 (length str) 'face 'transient-heading str)
                          str)
                        (run-spec-handlers)
                        )
(transient-make-call!   general-insert-rebuild-cache "@"
                        (let ((str  "Clear General-Insert Cache"))
                          (put-text-property 0 (length str) 'face 'transient-heading str)
                          str)
                        (general-insert-clear-caches)
                        (message "Cache Rebuilt")
                        )

;; Visual
(progn
  (transient-make-mode-toggle! hl-line-mode                   "Highlight-line"    "h")
  (transient-make-mode-toggle! evil-goggles-mode              "Evil-goggles"      "g")
  (transient-make-mode-toggle! highlight-parentheses-mode     "Higlight-wrappers" "w")
  (transient-make-mode-toggle! rainbow-mode                   "Rainbow Colours"   "r")
  (transient-make-mode-toggle! highlight-changes-visible-mode "Highlight-changes" "x")
  (transient-make-mode-toggle! reveal-mode                    "Reveal Invisible" "I")
  (transient-make-mode-toggle! auto-highlight-symbol-mode     "Auto Highlight Symbol" "H")

  ;;
  (transient-make-call! elide-head "e"
                        (format "%-2s : Elide Head" (fmt-as-bool! (if (boundp 'elide-head-overlay)
                                                                    elide-head-overlay
                                                                  "-1")))
                        (if (boundp 'elide-head-overlay) (elide-head-show) (elide-head))
                        )

  (transient-make-call! link-display "l"
                        (format "%-2s : Link Display" (fmt-as-bool! org-link-descriptive))
                        (org-toggle-link-display))
  (transient-make-call!  quickscope "s"
                         (format "%-2s : Quickscope"
                                 (fmt-as-bool! evil-quickscope-always-mode))
                         (evil-quickscope-always-mode 'toggle)
                         (evil-quickscope-mode (if evil-quickscope-always-mode -1 1))
                         )

  (transient-make-var-toggle!   invisible line-move-ignore-invisible "Invisible Spec" "i")
  )

(transient-make-subgroup! jg-toggle-visuals "v"
                          "For controlling ui visual settings"
                          :desc "|| Visuals    ||"
                          [[
                            (transient-macro-toggle-evil-goggles-mode)
                            (transient-macro-toggle-hl-line-mode)
                            (transient-macro-toggle-invisible)
                            (transient-macro-toggle-highlight-changes-visible-mode)
                            (transient-macro-call-link-display)
                            ]
                           [
                            (transient-macro-toggle-rainbow-mode)
                            (transient-macro-toggle-global-prettify-symbols-mode)
                            (transient-macro-call-quickscope)
                            (transient-macro-toggle-highlight-parentheses-mode)
                            (transient-macro-call-elide-head)
                            ]
                           [
                            (transient-macro-toggle-auto-highlight-symbol-mode)
                            ]
                           ]
                          )

;; Guides
(progn
  (transient-make-mode-toggle! display-fill-column-indicator-mode  (format "Columns : %-3s" fill-column) "c")
  (transient-make-mode-toggle! highlight-indent-guides-mode        "Indents"      "i")
  (transient-make-mode-toggle! ruler-mode                          "Ruler"        "r")
  (transient-make-mode-toggle! whitespace-mode                     "Whitespace"   "w")
  (transient-make-mode-toggle! display-line-numbers-mode           "Line Numbers" "n")
  (transient-make-mode-toggle! glasses-mode                        "Glasses" "g")

  ;;
  (transient-make-call!   spelling "s"
                          (format "%-2s : Spelling" (fmt-as-bool! flyspell-mode))
                          (flyspell-mode 'toggle)
                          (writegood-mode (if flyspell-mode 1 -1))
                          )

  )
(transient-make-subgroup! jg-toggle-guides "g"
                          "For controlling ui guide settings"
                          :desc "|| Guides     ||"
                          [[
                            (transient-macro-toggle-display-fill-column-indicator-mode)
                            (transient-macro-toggle-highlight-indent-guides-mode)
                            (transient-macro-toggle-display-line-numbers-mode)
                            ]
                           [
                            (transient-macro-toggle-ruler-mode)
                            (transient-macro-toggle-whitespace-mode)
                            (transient-macro-call-spelling)
                            ]
                           [
                            (transient-macro-toggle-glasses-mode)
                            ]
                           ]
                          )

;; Nav
(progn
  (transient-make-mode-toggle! centered-cursor-mode   "Center Cursor" "c")
  (transient-make-mode-toggle! minimap-mode           "Minimap"       "m")
  (transient-make-mode-toggle! evil-visual-mark-mode  "visual mark"   "v")

  ;; TODO add sidebar selection
  (transient-make-call! sidebar "s" "Sidebar"    (+jg-ui-tree/open))
  (transient-make-call! frame-fullscreen "f" "Fullscreen" (toggle-frame-fullscreen))

  (transient-make-var-toggle! auto-balance evil-auto-balance-windows "Auto-Balance Windows" "b")

  )
(transient-make-subgroup! jg-toggle-nav "n"
                          "For controlling ui nav settings"
                          :desc "|| Navigation ||"
                          [ [
                             (transient-macro-toggle-centered-cursor-mode)
                             (transient-macro-toggle-auto-balance)
                             (transient-macro-toggle-minimap-mode)
                             (transient-macro-toggle-evil-visual-mark-mode)
                             ] [
                             (transient-macro-call-sidebar)
                             (transient-macro-call-frame-fullscreen)
                             ] [
                             ] ]
                          )

;; Wrap
(progn
  (transient-make-mode-toggle! visual-line-mode  "Visual line" "l")
  (transient-make-mode-toggle! +word-wrap-mode   "Word-wrap"   "w")
  (transient-make-mode-toggle! auto-fill-mode    "Auto-fill"   "f" nil auto-fill-function)

  (transient-make-call! truncate-lines "t"
                        (format "%-2s : Truncate lines" (fmt-as-bool! truncate-lines))
                        (toggle-truncate-lines)
                        )

  )
(transient-make-subgroup! jg-toggle-wrap "w"
                          "For controlling ui wrap settings"
                          :desc "|| Wrapping   ||"
                          [[
                            (transient-macro-toggle-auto-fill-mode)
                            (transient-macro-toggle-visual-line-mode)
                            (transient-macro-toggle-+word-wrap-mode)
                            (transient-macro-call-truncate-lines)
                            ]
                           ]
                          )

;; Debug
(progn
  (transient-make-call! debug-on-error "e"
                        (format "%-2s : Debug on Error" (fmt-as-bool! debug-on-error))
                        (toggle-debug-on-error))
  (transient-make-call! debug-on-var "v"
                        (format "%-2s : Debug on Variable" (fmt-as-bool! (debug--variable-list)))
                        (call-interactively #'debug-on-variable-change))
  (transient-make-call! cancel-debug-on-var "V"
                        "Cancel All Var Debugs"
                        (cancel-debug-on-variable-change))
  (transient-make-call! debug-func "f"
                        (format "%-2s : Debug on Function" (fmt-as-bool! (debug--function-list)))
                        (call-interactively #'debug-on-entry))
  (transient-make-call! cancel-debug-func "F"
                        "Cancel All Function Debugs"
                        (cancel-debug-on-entry))
  )
(transient-make-subgroup! jg-toggle-debugs "d"
                          " debug toggles "
                          :desc "|| Debug      ||"
                          [ [
                             (transient-macro-call-debug-on-error)
                             (transient-macro-call-debug-on-var)
                             (transient-macro-call-debug-func)
                             ]
                            [
                             " "
                             (transient-macro-call-cancel-debug-on-var)
                             (transient-macro-call-cancel-debug-func)

                             ] ]
                          )

;; Top Level Toggle
(progn
  (transient-make-mode-toggle! global-hl-line-mode            "Hl-line"       "h")
  (transient-make-mode-toggle! hide-mode-line-mode            "Hide Modeline" "m")
  (transient-make-mode-toggle! global-code-shy-minor-mode     "Shy Code"      "H")
  (transient-make-mode-toggle! global-centered-cursor-mode    "Center Cursor" "c")
  (transient-make-mode-toggle! global-highlight-changes-mode  "Show Changes"  "x")
  (transient-make-mode-toggle! smartparens-global-mode        "SmartParens"   "s")
  (transient-make-mode-toggle! abbrev-mode                    "Abbrev"        "a")
  )

;;;###autoload (autoload #'jg-toggle-main "config/ui/autoload/transient-toggles" nil t)
(transient-define-prefix jg-toggle-main ()
  "Main controller for ui settings"
  [
   (transient-macro-call-run-spec-handlers)
   (transient-macro-call-general-insert-rebuild-cache)
   ]
  [ [
     jg-toggle-debugs
     jg-toggle-guides
     jg-toggle-nav
     jg-toggle-visuals
     jg-toggle-wrap
     ]
    [
     (transient-macro-toggle-global-hl-line-mode)
     (transient-macro-toggle-hide-mode-line-mode)
     (transient-macro-toggle-global-prettify-symbols-mode)
     (transient-macro-toggle-global-highlight-changes-mode)
     (transient-macro-toggle-abbrev-mode)
     ] [
     (transient-macro-toggle-global-code-shy-minor-mode)
     (transient-macro-toggle-smartparens-global-mode)
     (transient-macro-toggle-global-centered-cursor-mode)
     ] [
     (transient-macro-toggle-read-only-mode)
     (transient-macro-call-evil-embrace)
     (transient-macro-call-debug-on-error)
     (transient-macro-toggle-global-company-mode)
     ] ]
  transient-quit!
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

(provide 'transient-toggles)
