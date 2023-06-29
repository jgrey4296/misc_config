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

(load! "+vars")
(defer-load! jg-bindings-total "+bindings")

;;-- lsp

(use-package! lsp-mode
  :commands (lsp-install-server lsp-deferred)
  :init
  ;; Don't touch ~/.emacs.d, which could be purged without warning
  (setq lsp-session-file (concat doom-cache-dir "lsp-session")
        lsp-server-install-dir (concat doom-data-dir "lsp")
        lsp-keymap-prefix nil)

  (spec-handling-add! python-env
                      `(lsp
                        (:support lsp
                                  ,#'(lambda (state) (add-hook 'python-mode-hook #'lsp-deferred))
                                  ,#'(lambda (state)
                                       (when lsp-mode
                                         (lsp-mode -1))
                                       (when lsp--last-active-workspaces
                                         (lsp-workspace-shutdown (car lsp--last-active-workspaces)))
                                       (remove-hook 'python-mode-hook #'lsp-deferred)
                                       )
                                  )
                        (:teardown lsp ,#'(lambda (state) (lsp-disconnect)))
                        )
                      )

  :config
  (add-to-list 'doom-debug-variables 'lsp-log-io)

  (setq lsp-intelephense-storage-path (concat doom-data-dir "lsp-intelephense/")
        lsp-vetur-global-snippets-dir
        (expand-file-name
         "vetur" (or (bound-and-true-p +snippets-dir)
                     (concat doom-user-dir "snippets/")))
        lsp-xml-jar-file (expand-file-name "org.eclipse.lsp4xml-0.3.0-uber.jar" lsp-server-install-dir)
        lsp-groovy-server-file (expand-file-name "groovy-language-server-all.jar" lsp-server-install-dir))

  ;; REVIEW Remove this once this is fixed upstream.
  (add-to-list 'lsp-client-packages 'lsp-racket)

  (add-hook! 'doom-escape-hook #'+lsp-signature-stop-maybe-h)
  (add-hook! 'lsp-mode-hook #'+lsp-optimization-mode)

)

(use-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  )

(use-package! lsp-ivy
  :when (modulep! :completion ivy)
  :commands lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol)

;;-- end lsp

;;-- eglot

(use-package! eglot
  :commands eglot eglot-ensure
  :hook (eglot-managed-mode . +lsp-optimization-mode)
  :init
  (spec-handling-add! python-env
                      `(eglot
                        (:support eglot
                                  ,#'(lambda (state) (add-hook 'python-mode-hook #'eglot-ensure))
                                  ,#'(lambda (state)
                                       (signal 'eglot-todo (current-buffer))
                                       (remove-hook 'python-mode-hook #'eglot-ensure)
                                       )
                                  )
                        )
                      )

  :config
  (add-to-list 'doom-debug-variables '(eglot-events-buffer-size . 0))

  )

(use-package! flycheck-eglot
  :when (modulep! :checkers syntax)
  :hook (eglot-managed-mode . flycheck-eglot-mode)
  )

;;-- end eglot

;;-- tree sitter

(use-package! tree-sitter
  :defer t
  :init
  (spec-handling-add! python-env
                      `(tree-sitter!
                        (:support tree-sitter
                                  ,#'(lambda (state) (add-hook 'python-mode-hook #'tree-sitter!))
                                  ,#'(lambda (state)
                                       (remove-hook 'python-mode-hook #'tree-sitter!))
                                  )
                        )
                      )

  (spec-handling-new! tree-sit-lang
                      tree-sitter-major-mode-language-alist
                      :loop 'collect
                      `(,key . ,val)
                      )
  :config
  (require 'tree-sitter-langs)
  )

(use-package! evil-textobj-tree-sitter
  :when (modulep! :editor evil +everywhere)
  :defer t
  :init (after! tree-sitter (require 'evil-textobj-tree-sitter))
  :config
  (after! which-key
    (setq which-key-allow-multiple-replacements t)
    (pushnew!
     which-key-replacement-alist
     '(("" . "\\`+?evil-textobj-tree-sitter-function--\\(.*\\)\\(?:.inner\\|.outer\\)") . (nil . "\\1"))))
  )

;;-- end tree sitter

;;-- semantic

(use-package! cedet)

(use-package! semantic
  :defer t
  :init
  (spec-handling-add! python-env
                      `(semantic
                        (:support semantic
                                  ,#'(lambda (state) (add-hook 'python-mode-hook #'semantic-mode))
                                  ,#'(lambda (state) (remove-hook 'python-mode-hook #'semantic-mode))
                                  )
                        )
                      )
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
  :commands flycheck-list-errors flycheck-buffer
  ;; :hook (doom-first-buffer . global-flycheck-mode)
  :init

  (spec-handling-add! python-env
                      '(flycheck
                        (:support flycheck #'(lambda (path name)
                                               (unless flycheck-enabled-checkers
                                                 (let ((chosen (intern (ivy-read "Flychecker: " flycheck-disabled-checkers :require-match t))))
                                                   (delete chosen flycheck-disabled-checkers)
                                                   (add-to-list flycheck-enabled-checkers chosen)
                                                   ))
                                               (add-hook 'python-mode-hook #'flycheck-mode)
                                               )
                                  (-partial #'flycheck-mode -1)
                                  )
                        (:teardown flycheck (-partial flycheck-mode -1))
                        )
                      )
  (setq flycheck-global-modes nil)

  ;; (spec-handling-new! flycheck nil :loop 'do


  ;;                     )
  ;; (spec-handling-new! flycheck-disabled nil :loop 'do

  ;;                     )

  :config
  (add-hook! 'doom-escape-hook :append #'+syntax-check-buffer-h)

  (remove-hook! 'after-change-major-mode-hook
    #'global-flycheck-mode-enable-in-buffers
    )
)

(use-package! flycheck-popup-tip
  :commands flycheck-popup-tip-show-popup flycheck-popup-tip-delete-popup
  :hook (flycheck-mode . +syntax-init-popups-h)
  :config
  (after! evil
    ;; Don't display popups while in insert or replace mode, as it can affect
    ;; the cursor's position or cause disruptive input delays.
    (add-hook! '(evil-insert-state-entry-hook evil-replace-state-entry-hook)
               #'flycheck-popup-tip-delete-popup)
    (defadvice! +syntax--disable-flycheck-popup-tip-maybe-a (&rest _)
      :before-while #'flycheck-popup-tip-show-popup
      (if evil-local-mode
          (eq evil-state 'normal)
        (not (bound-and-true-p company-backend)))))
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

;;; config.el ends here
