;;; editor/window-control/+vars.el -*- lexical-binding: t; -*-

(defvar mouse-wheel-down-event nil)
(defvar mouse-wheel-up-event nil)
(defvar jg-ui-default-face-gen-palette-dir "/Volumes/documents/github/jgrey4296.github.io/resources/palettes/")

(setq-default highlight-parentheses-delay      0.3
              display-line-numbers             t
              display-line-numbers-major-tick  20
              display-line-numbers-width 4
              display-line-numbers-type t

              overflow-newline-into-fringe t

              highlight-parentheses-colors            '("black")
              highlight-parentheses-background-colors '("#60aa00" "yellow" "#da8548" "#d02b61")
              global-hl-line-modes '(bibtex-mode prog-mode text-mode conf-mode special-mode org-agenda-mode comint-mode)

              line-move-ignore-invisible t
              avy-all-windows t

              whitespace-style '(face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark)

              which-key-idle-secondary-delay 0.05
              which-key-sort-order 'which-key-key-order-alpha
              )

;;-- hl todo
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
;;-- end hl todo

;;-- highlight indent guides
(setq highlight-indent-guides-method 'character
      highlight-indent-guides-suppress-auto-error t
      )
;;-- end highlight indent guides

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

;;-- so long
(setq so-long-threshold 5000)
;;-- end so long

(spec-handling-add! auto-modes
                    '(ui
                      ("\\.palette" . palette-mode)
                      )
                    )
