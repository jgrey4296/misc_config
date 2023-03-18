;;; editor/window-control/+vars.el -*- lexical-binding: t; -*-

(defvar jg-ui-reapply-hook nil)

(setq-default highlight-parentheses-delay      0.3
              jg-ui-default-face-gen-palette-dir "/Volumes/documents/github/jgrey4296.github.io/resources/palettes/"
              display-line-numbers             t
              display-line-numbers-major-tick  20
)

;;-- smartparens
(setq-default ;; smartparens
              sp-autoinsert-pair t
              sp-autoinsert-quote-if-followed-by-closing-pair nil
              sp-escape-char ""
              )

;;-- end smartparens

;;-- parens
(setq-default
              highlight-parentheses-colors            '("black")
              highlight-parentheses-background-colors '("#60aa00" "yellow" "#da8548" "#d02b61")
              global-hl-line-modes '(bibtex-mode prog-mode text-mode conf-mode special-mode org-agenda-mode comint-mode)
              )

;;-- end parens
