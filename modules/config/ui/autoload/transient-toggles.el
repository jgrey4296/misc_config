;;; transient-toggles.el -*- lexical-binding: t; -*-
(require 'transient)

;; Visual
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
  transient-quit!
  )

;; Guides
(progn
  (transient-make-toggle! display-fill-column-indicator-mode "c" "Columns")
  (transient-make-toggle! highlight-indent-guides-mode "i" "Indents")
  (transient-make-toggle! ruler-mode "r" "Ruler")
  (transient-make-toggle! whitespace-mode "w" "Whitespace")
  (transient-make-call!   spelling
                          (format "%s : Spelling" (fmt-as-bool flyspell-mode))
                          (flyspell-mode 'toggle)
                          (writegood-mode (unless flyspell-mode -1))
                          )

  )
(transient-define-prefix jg-toggle-guides ()
  "For controlling ui guide settings"
  [[
    (jg-transient-toggle-display-fill-column-indicator-mode)
    (jg-transient-toggle-highlight-indent-guides-mode)
    (jg-transient-toggle-ruler-mode)
    (jg-transient-toggle-whitespace-mode)
    ("s" jg-transient-call-spelling)
   ]
  ]
  transient-quit!
  )

;; Wrap
(progn
  (transient-make-toggle! visual-line-mode "l" "visual line")
  (transient-make-toggle! +word-wrap-mode "w" "Word-wrap")
  (transient-make-call! truncate-lines
                        (format "%s : truncate lines" (fmt-as-bool truncate-lines))
                        (toggle-truncate-lines)
                        )
  (transient-make-call! auto-fill-mode
                        (format "%s : auto-fill" (fmt-as-bool
                                                  auto-fill-function))
                        (auto-fill-mode 'toggle)
                        )
  )
(transient-define-prefix jg-toggle-wrap ()
  "For controlling ui wrap settings"
  [[
    ("f" jg-transient-call-auto-fill-mode)
    (jg-transient-toggle-visual-line-mode)
    (jg-transient-toggle-+word-wrap-mode)
    ("t" jg-transient-call-truncate-lines)
   ]
  ]
  transient-quit!
  )

;; Nav
(progn
  (transient-make-toggle! global-autohide-minor-mode "a" "Autohide")
  (transient-make-toggle! centered-cursor-mode "c" "Center Cursor")
  (transient-make-toggle! minimap-mode "m" "minimap")
  (transient-make-toggle! evil-visual-mark-mode "v" "visual mark")
  (transient-make-call! neotree
                        "neotree"
                        (neotree-toggle)
                        )
  (transient-make-call! frame-fullscreen
                        (format "Fullscreen")
                        (toggle-frame-fullscreen))
  (transient-make-call! auto-balance
                        (format "%s : Auto-Balance Windows" (fmt-as-bool evil-auto-balance-windows))
                        (setq evil-auto-balance-windows
                              (not evil-auto-balance-windows))
                        )

  )
(transient-define-prefix jg-toggle-nav ()
  "For controlling ui nav settings"
  [[
    (jg-transient-toggle-global-autohide-minor-mode)
    (jg-transient-toggle-centered-cursor-mode)
    (jg-transient-toggle-minimap-mode)
   ]
   [
    (jg-transient-toggle-evil-visual-mark-mode)
    ("b" jg-transient-call-auto-balance)
    ]
   [
    ("n" jg-transient-call-neotree)
    ("f" jg-transient-call-frame-fullscreen)
    ]
   ]
  transient-quit!
  )

;; Top Level Toggle
(progn
  (transient-make-toggle! hide-mode-line-mode "m" "Modeline")
  (transient-make-toggle! global-hl-line-mode "h" "Hi-line")
  (transient-make-toggle! global-autohide-minor-mode "a" "Autohide")
  (transient-make-toggle! global-centered-cursor-mode "c" "Cursor")
  (transient-make-toggle! global-highlight-changes-mode "l" "Changes")

  )
;;TODO apply choice
(transient-define-argument browse-selector ()
  :class 'transient-switches
  :argument-format "-browser=%s"
  :argument-regexp "-browser=%s"
  :choices browse-select-variants
  )

;;;###autoload
(transient-define-prefix jg-toggle-main ()
  "Main controler for ui settings"
  [
   [
   (jg-transient-toggle-hide-mode-line-mode)
   (jg-transient-toggle-global-hl-line-mode)
   (jg-transient-toggle-global-autohide-minor-mode)
   ]
   [
    (jg-transient-toggle-global-centered-cursor-mode)
    (jg-transient-toggle-global-highlight-changes-mode)
    ]
   [
    ("v" "Visuals" jg-toggle-visuals)
    ("g" "Guides" jg-toggle-guides)
    ("n" "Navigation" jg-toggle-nav)
    ("w" "Wrapping" jg-toggle-wrap)
    ]
   ]
  transient-quit!
  )
