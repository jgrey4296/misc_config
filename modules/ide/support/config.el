;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 John Grey
;;
;; Author: John Grey <https://github.com/jgrey4296>
;; Maintainer: John Grey <johngrey4296 at gmail.com>
;; Created: April 07, 2023
;; Modified: April 07, 2023
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jgrey4296
;; Package-Requires: ((emacs "24.3"))
;; Package written on: ((emacs 28.2))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(local-load! "+defs")
(local-load! "+vars")
(local-load! "+spec-defs")

(defer-load! jg-bindings-total "+bindings")

(defer-load! jg-evil-ex-bindings "+evil-ex")

(after! transient-toggles (+jg-ide-extend-toggles))

(advice-add '+eval--ensure-in-repl-buffer    :filter-return #'+jg-repl-fix)
(advice-add '+jg-send-region-to-repl         :filter-args #'+jg-advice-send-repl-auto-line)
(advice-add 'lsp-diagnostics-flycheck-enable :around #'+lsp--respect-user-defined-checkers-a)
(advice-add 'lsp-describe-session            :around #'+jg-lsp-dont-select-session)
(advice-add 'lsp--shutdown-workspace         :around #'+lsp-defer-server-shutdown-a)
(advice-add 'lsp--auto-configure             :around #'+lsp--use-hook-instead-a)
(advice-add 'eglot--managed-mode             :around #'+lsp--defer-server-shutdown-a)
(advice-add 'lsp-diagnostics--flycheck-level :before #'+lsp--log-diagnostic-build)
;;-- lsp

(use-package! lsp-mode
  :commands (lsp-install-server lsp-deferred lsp-update-servers)
  :init
  ;; Don't touch ~/.emacs.d, which could be purged without warning
  (setq lsp-session-file (concat doom-cache-dir "lsp-session")
        lsp-server-install-dir (concat doom-data-dir "lsp")
        lsp-keymap-prefix nil)


  ;; override what is auto loaded
  (setq lsp-client-packages nil)

  :config
  (add-to-list 'doom-debug-variables 'lsp-log-io)

  (setq lsp-xml-jar-file (expand-file-name "org.eclipse.lsp4xml-0.3.0-uber.jar" lsp-server-install-dir)
        lsp-groovy-server-file (expand-file-name "groovy-language-server-all.jar" lsp-server-install-dir))

  (add-hook! 'lsp-mode-hook #'+lsp-optimization-mode)

)

(use-package! lsp-ui
  :commands (lsp-ui-doc-mode lsp-ui-imenu lsp-ui-sideline)
  )

(use-package! lsp-ivy
  :commands lsp-ivy--transform-candidate)

;;-- end lsp

;;-- eglot

(use-package! eglot
  :commands (eglot eglot-ensure)
  :hook (eglot-managed-mode . +lsp-optimization-mode)
  :config
  (add-to-list 'doom-debug-variables '(eglot-events-buffer-size . 0))

  )

(use-package! flycheck-eglot
  :after eglot
  :hook (eglot-managed-mode . flycheck-eglot-mode)
  )

;;-- end eglot

;;-- semantic

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

;;-- end semantic

;;-- flycheck

(use-package! flycheck
  :commands (flycheck-list-errors flycheck-buffer flycheck-mode global-flycheck-mode)
  ;; :hook (doom-first-buffer . global-flycheck-mode)
  :init
  (setq flycheck-global-modes nil)

  :config
  (after! fringe
    ;; Let diff-hl have left fringe, flycheck can have right fringe
    ;; A non-descript, left-pointing arrow
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow [16 48 112 240 112 48 16] nil nil 'center)
    )

  (delq 'new-line flycheck-check-syntax-automatically)
  (remove-hook 'after-change-major-mode-hook #'global-flycheck-mode-enable-in-buffers)

)

(use-package! flycheck-popup-tip
  :commands flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup
  :hook (flycheck-mode . flycheck-popup-tip-mode)
  :config
  (after! evil
    ;; Don't display popups while in insert or replace mode, as it can affect
    ;; the cursor's position or cause disruptive input delays.
    (add-hook! '(evil-insert-state-entry-hook evil-replace-state-entry-hook)
               #'flycheck-popup-tip-delete-popup)
    (advice-add 'flycheck-popup-tip-show-popup :before-while #'+syntax--disable-flycheck-popup-tip-maybe-a)
    )
  )

(use-package! flycheck-posframe
  :when (modulep! +childframe)
  :hook (flycheck-mode . +syntax-init-popups-h)
  :config
  (after! company
    ;; Don't display popups if company is open
    (add-hook 'flycheck-posframe-inhibit-functions #'company--active-p))
  (after! evil
    ;; Don't display popups while in insert or replace mode, as it can affect
    ;; the cursor's position or cause disruptive input delays.
    (add-hook! 'flycheck-posframe-inhibit-functions
               #'evil-insert-state-p
               #'evil-replace-state-p))
  )

;;-- end flycheck

(use-package! tree-sitter ;; Melpas
  ;; :disabled t
  :defer t
  :config
  (require 'tree-sitter-langs)
  )

(use-package! treesit ;; builtin
  :disabled t
  :defer t
  :config
  ;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
  )

(use-package! tree-sitter-langs
  :defer t
  :config
  ;; possibly: (cl-pushnew (tree-sitter-lands--bin-dir) tree-sitter-load-path :test #'string-equal)
  )

(use-package! lint-result-mode
  :config
  (add-hook 'lint-result-mode-hook '+fold/close-all)
  )

;;; config.el ends here
