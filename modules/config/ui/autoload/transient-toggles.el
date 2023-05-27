;;; transient-toggles.el -*- lexical-binding: t; -*-
(require 'transient)

;; Suffixes
(progn
  (transient-make-toggle! hl-line-mode "h" "hl-line")
  (transient-make-toggle! evil-goggles-mode "g" "evil-goggles")
  (transient-make-toggle! highlight-parentheses-mode "w" "higlight-wrappers")
  (transient-make-toggle! rainbow-mode "c" "rainbow")
  (transient-make-toggle! prettify-symbols-mode "p" "pretty-symbols")
  (transient-make-toggle! highlight-changes-visible-mode "l" "highlight-changes")

  (transient-make-call!   quickscope
                          (format "%s/%s : quickscope"
                                  (fmt-as-bool evil-quickscope-always-mode)
                                  (fmt-as-bool evil-quickscope-mode))
                          (evil-quickscope-always-mode 'toggle)
                          (evil-quickscope-mode (if evil-quickscope-always-mode -1 1))
                          )
  (transient-make-call!   invisible
                          (format "%s : invisible" (fmt-as-bool line-move-ignore-invisible))
                          (setq line-move-ignore-invisible (not line-move-ignore-invisible))
                          )
  )

;; Visual
(transient-define-prefix jg-toggle-visuals ()
  "For controlling ui visual settings"
  [[
   (jg-transient-toggle-evil-goggles-mode)
   (jg-transient-toggle-hl-line-mode)
   (jg-transient-toggle-highlight-changes-visible-mode)
   ("i" jg-transient-call-invisible)
   ]
  [
   (jg-transient-toggle-highlight-parentheses-mode)
   (jg-transient-toggle-rainbow-mode)
   (jg-transient-toggle-prettify-symbols-mode)
   ("s" jg-transient-call-quickscope)
   ]
  ]
  [
   ""
   ("q" "Quit" transient-quit-one)
   ]
  )

;; Guides
(progn
  (transient-make-toggle! display-fill-column-indicator-mode)
  (transient-make-toggle! highlight-indent-guides-mode)
  (transient-make-toggle! rule-mode)
  (transient-make-toggle! whitespace-mode)
  (transient-make-call!   spelling
                          (format "")
                          )

  )
(transient-define-prefix jg-toggle-guides ()
  "For controlling ui guide settings"
  [[
   ]
  [
   ]
  ]
  [
   ""
   ("q" "Quit" transient-quit-one)
   ]
  )

;; Wrap
(progn
  (transient-make-toggle! auto-fill-mode)
  (transient-make-toggle! visual-line-mode)
  (transient-make-toggle! +word-wrap-mode)
  (transient-make-call! truncate-lines
                        (format "")
                        (toggle-truncate-lines)
                        )
  )
(transient-define-prefix jg-toggle-wrap ()
  "For controlling ui wrap settings"
  [[
   ]
  [
   ]
  ]
  [
   ""
   ("q" "Quit" transient-quit-one)
   ]
  )

;; Nav
(progn
  (transient-make-toggle! global-autohide-minor-mode)
  (transient-make-toggle! centered-cursor-mode)
  (transient-make-toggle! minimap-mode)
  (transient-make-toggle! neotree-toggle)
  (transient-make-toggle! evil-visual-mark-mode)
  (transient-make-call! frame-fullscreen
                        (format "")
                        (toggle-frame-fullscreen))
  (transient-make-call! auto-balance
                        (format "")
                        (setq evil-auto-balance-windows
                              (not evil-auto-balance-windows))
                        )

  )
(transient-define-prefix jg-toggle-nav ()
  "For controlling ui nav settings"
  [[
   ]
  [
   ]
  ]
  [
   ""
   ("q" "Quit" transient-quit-one)
   ]
  )

;; Top Level Toggle
(progn
  (transient-make-toggle! hide-mode-line-mode)
  (transient-make-toggle! global-hl-line-mode)
  (transient-make-toggle! global-autohide-minor-mode)
  (transient-make-toggle! global-centered-cursor-mode)
  (transient-make-toggle! global-highlight-changes-mode)

  )
;;TODO apply choice
(transient-define-argument browse-selector ()
  :class 'transient-switches
  :argument-format "-browser=%s"
  :argument-regexp "-browser=%s"
  :choices browse-select-variants
  )
