;;; +semantic.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;-- Header
;; File Commentary:
;;
;;
;; See footer for licenses/metadata/notes as applicable
;;-- end Header

;; (use-package! cedet)

(use-package! semantic
  :commands semantic-mode
  :init
  :config
  ;;  global-semanticdb-minor-mode        - Maintain tag database.
  ;;  global-semantic-idle-scheduler-mode - Reparse buffer when idle.
  ;;  global-semantic-idle-summary-mode   - Show summary of tag at point.
  ;;  global-semantic-idle-completions-mode - Show completions when idle.
  ;;  global-semantic-decoration-mode     - Additional tag decorations.
  ;;  global-semantic-highlight-func-mode - Highlight the current tag.
  ;;  global-semantic-stickyfunc-mode     - Show current fun in header line.
  ;;  global-semantic-mru-bookmark-mode   - Provide switch-to-buffer-like keybinding for tag names.
  ;;  global-semantic-idle-local-symbol-highlight-mode - Highlight references of the symbol under point.
  ;;
  ;;  For internals of the semantic parser in action:
  ;;  global-semantic-highlight-edits-mode - Visualize incremental parser by highlighting not-yet parsed changes.
  ;;  global-semantic-show-unmatched-syntax-mode - Highlight unmatched lexical syntax tokens.
  ;;  global-semantic-show-parser-state-mode - Display the parser cache state.
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  (add-to-list 'semantic-default-submodes 'semantic-stickyfunc-mode)
  (add-to-list 'semantic-default-submodes 'semantic-highlight-func-mode)
  (add-to-list 'semantic-new-buffer-setup-functions '(emacs-lisp-mode . semantic-default-elisp-setup))

  )

(speckler-add! lib-env ()
  '(semantic
    :start #'(lambda (state &rest rest) (add-hook 'python-mode-hook #'semantic-mode))
    :stop  #'(lambda (state &rest rest) (remove-hook 'python-mode-hook #'semantic-mode))
    )
  )
)
;;-- Footer
;; Copyright (C) 2024 john
;;
;; Author:     john <https://github.com/jgrey4296>
;; Maintainer: john <john@john-UM700>
;; Created:    September 10, 2024
;; Modified:   September 10, 2024
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 29.1))
;;
;; This file is not part of GNU Emacs.
;;
;;-- end Footer
;;; +semantic.el ends here
