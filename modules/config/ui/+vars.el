;;; editor/window-control/+vars.el -*- lexical-binding: t; -*-

(defvar jg-ui-default-face-gen-palette-dir (expand-file-name "~/.config/jg/templates/palettes/"))
(defvar jg-ui-tree-active-tree-package 'neotree)

;;-- theme settings
(setq custom-theme-directory (expand-file-name "templates/themes" doom-user-dir))

;;-- end theme settings

(setq confirm-kill-emacs #'doom-quit-p
      confirm-nonexistent-file-or-buffer nil
      uniquify-buffer-name-style 'forward
      ring-bell-function #'ignore

      visible-bell nil


      ;; Transient
      ;; Must be set early to prevent ~/.config/emacs/transient from being created
      transient-levels-file  (concat doom-data-dir "transient/levels")
      transient-values-file  (concat doom-data-dir "transient/values")
      transient-history-file (concat doom-data-dir "transient/history")

      whitespace-style '(face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark)
      whitespace-line-column nil
      whitespace-display-mappings '((tab-mark ?\t [?› ?\t])
                                    (newline-mark ?\n [?¬ ?\n])
                                    (space-mark ?\  [?·] [?.]))

      )

;;-- highlighting
(setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(;; For reminders to change or add something at a later date.
          ("TODO" warning bold)
          ;; For code (or code paths) that are broken, unimplemented, or slow,
          ;; and may become bigger problems later.
          ("FIXME" error bold)
          ;; For code that needs to be revisited later, either to upstream it,
          ;; improve it, or address non-critical issues.
          ("REVIEW" font-lock-keyword-face bold)
          ;; For code smells where questionable practices are used
          ;; intentionally, and/or is likely to break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For sections of code that just gotta go, and will be gone soon.
          ;; Specifically, this means the code is deprecated, not necessarily
          ;; the feature it enables.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; Extra keywords commonly found in the wild, whose meaning may vary
          ;; from project to project.
          ("NOTE" success bold)
          ("BUG" error bold)
          ("XXX" font-lock-constant-face bold))
        )

(setq highlight-indent-guides-method 'character
      highlight-indent-guides-suppress-auto-error t

      highlight-parentheses-delay      0.3
      highlight-parentheses-colors            '("black")
      highlight-parentheses-background-colors '("#60aa00" "yellow" "#da8548" "#d02b61")

      global-hl-line-modes '(bibtex-mode prog-mode text-mode conf-mode special-mode org-agenda-mode comint-mode)
      )
;;-- end highlighting

;;-- modeline
;; We display project info in the modeline ourselves
(setq projectile-dynamic-mode-line nil
      ;; Set these early so they don't trigger variable watchers
      doom-modeline-bar-width 3
      doom-modeline-github          nil
      doom-modeline-mu4e            nil
      doom-modeline-persp-name      nil
      doom-modeline-minor-modes     nil
      doom-modeline-major-mode-icon nil
      doom-modeline-buffer-file-name-style 'relative-from-project
      ;; Only show file encoding if it's non-UTF-8 and different line endings
      ;; than the current OSes preference
      doom-modeline-buffer-encoding 'nondefault
      doom-modeline-default-eol-type (cond (IS-MAC 2)
                                           (IS-WINDOWS 1)
                                           (0))

      doom-modeline-icon nil
      )

;;-- end modeline

;;-- treemacs
(setq treemacs-collapse-dirs 3

 )
;;-- end treemacs

;;-- neotree
(setq neo-create-file-auto-open nil
      neo-toggle-window-keep-p t
      neo-auto-indent-point nil
      neo-autorefresh nil
      neo-mode-line-type 'none
      neo-window-width 30
      neo-show-updir-line nil
      neo-theme 'icons
      neo-banner-message nil
      neo-confirm-create-file #'off-p
      neo-confirm-create-directory #'off-p
      neo-show-hidden-files nil
      neo-keymap-style 'concise
      )
(after! 'dired-omit-files-set
  (setq neo-hidden-regexp-list (list dired-omit-files))
  )

;;-- end neotree

;;-- go away mouse
(setq mouse-yank-at-point nil
      mouse-wheel-scroll-amount nil
      mouse-wheel-scroll-amount-horizontal 2
      )

;;-- end go away mouse

(spec-handling-add! auto-modes
                    '(ui
                      ("\\.palette" . palette-mode)
                      )
                    )

(spec-handling-add! popup
                    '(ui
                     ("^ \\*Treemacs"         :side left :ttl 5   :width 0.2 :quit t :select nil :priority 50)
                     ("\\*NeoTree\\*"   :side left :ttl nil :height 0.4 :quit nil :select nil :priority 100)
                     )
                    )

(spec-handling-add! fold
                    '(neotree
                     :modes (neotree-mode)
                     :priority 25
                     :triggers (:open-all   nil
                                :close-all  neotree-collapse-all
                                :toggle     nil
                                :open       +neotree/expand-or-open
                                :open-rec   nil
                                :close      +neotree/collapse
                                )
                     )
                    )
